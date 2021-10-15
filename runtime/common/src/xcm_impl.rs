// This file is part of Bifrost.

// Copyright (C) 2019-2021 Liebi Technologies (UK) Ltd.
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

use codec::{Decode, Encode};
pub use cumulus_primitives_core::ParaId;
use frame_support::{
	sp_runtime::traits::{CheckedConversion, Convert},
	traits::{Contains, Get},
	weights::Weight,
};
use node_primitives::{AccountId, CurrencyId, TokenSymbol};
use orml_traits::location::Reserve;
use polkadot_parachain::primitives::Sibling;
use sp_core::H160;
use sp_std::{convert::TryFrom, marker::PhantomData};
use xcm::latest::prelude::*;
use xcm_builder::{AccountId32Aliases, NativeAsset, ParentIsDefault, SiblingParachainConvertsVia};
use xcm_executor::traits::{FilterAssetLocation, MatchesFungible, ShouldExecute};

use crate::constants::parachains;

/// Bifrost Asset Matcher
pub struct BifrostAssetMatcher<CurrencyId, CurrencyIdConvert>(
	PhantomData<(CurrencyId, CurrencyIdConvert)>,
);

impl<CurrencyId, CurrencyIdConvert, Amount> MatchesFungible<Amount>
	for BifrostAssetMatcher<CurrencyId, CurrencyIdConvert>
where
	CurrencyIdConvert: Convert<MultiLocation, Option<CurrencyId>>,
	Amount: TryFrom<u128>,
{
	fn matches_fungible(a: &MultiAsset) -> Option<Amount> {
		if let (Fungible(ref amount), Concrete(ref location)) = (&a.fun, &a.id) {
			if CurrencyIdConvert::convert(location.clone()).is_some() {
				return CheckedConversion::checked_from(*amount);
			}
		}
		None
	}
}

/// Bifrost Location Convert
pub type BifrostLocationConvert = (
	// The parent (Relay-chain) origin converts to the default `AccountId`.
	ParentIsDefault<AccountId>,
	// Sibling parachain origins convert to AccountId via the `ParaId::into`.
	SiblingParachainConvertsVia<Sibling, AccountId>,
	// Straight up local `AccountId32` origins just alias directly to `AccountId`.
	AccountId32Aliases<NetworkId, AccountId>,
);

/// Bifrost Xcm Transact Filter
pub struct BifrostXcmTransactFilter<T>(PhantomData<T>);

impl<T: Contains<MultiLocation>> ShouldExecute for BifrostXcmTransactFilter<T> {
	fn should_execute<Call>(
		_origin: &MultiLocation,
		_top_level: bool,
		message: &Xcm<Call>,
		_shallow_weight: Weight,
		_weight_credit: &mut Weight,
	) -> Result<(), ()> {
		match message {
			Xcm::Transact { origin_type: _, require_weight_at_most: _, call: _ } => Ok(()),
			_ => Err(()),
		}
	}
}

/// Bifrost Filtered Assets
pub struct BifrostFilterAsset;

impl FilterAssetLocation for BifrostFilterAsset {
	fn filter_asset_location(asset: &MultiAsset, origin: &MultiLocation) -> bool {
		if let Some(ref reserve) = asset.reserve() {
			if reserve == origin {
				return true;
			}
		}
		false
	}
}

pub type BifrostFilteredAssets = (NativeAsset, BifrostFilterAsset);

fn native_currency_location(id: CurrencyId, para_id: ParaId) -> MultiLocation {
	MultiLocation::new(1, X2(Parachain(para_id.into()), GeneralKey(id.encode())))
}

pub struct BifrostCurrencyIdConvert<T, H>(sp_std::marker::PhantomData<(T, H)>);
impl<T: Get<ParaId>, H: Get<H160>> Convert<CurrencyId, Option<MultiLocation>>
	for BifrostCurrencyIdConvert<T, H>
{
	fn convert(id: CurrencyId) -> Option<MultiLocation> {
		use CurrencyId::{Native, Stable, Token, VSToken};
		match id {
			Token(TokenSymbol::KSM) => Some(MultiLocation::parent()),
			Native(TokenSymbol::ASG) | Native(TokenSymbol::BNC) | VSToken(TokenSymbol::KSM) =>
				Some(native_currency_location(id, T::get())),
			// Karura currencyId types
			Token(TokenSymbol::KAR) => Some(MultiLocation::new(
				1,
				X2(
					Parachain(parachains::karura::ID),
					GeneralKey(parachains::karura::KAR_KEY.to_vec()),
				),
			)),
			Stable(TokenSymbol::KUSD) => Some(MultiLocation::new(
				1,
				X2(
					Parachain(parachains::karura::ID),
					GeneralKey(parachains::karura::KUSD_KEY.to_vec()),
				),
			)),
			// SnowBridge currencyId types
			Token(TokenSymbol::ETH) => Some(MultiLocation::new(
				1,
				X2(
					Parachain(parachains::snowfork::ID),
					GeneralKey(parachains::snowfork::AssetId::ETH.encode()),
				),
			)),
			Token(TokenSymbol::VETH) => Some(MultiLocation::new(
				1,
				X2(
					Parachain(parachains::snowfork::ID),
					GeneralKey(parachains::snowfork::AssetId::Token(H::get()).encode()),
				),
			)),
			_ => None,
		}
	}
}

impl<T: Get<ParaId>, H: Get<H160>> Convert<MultiLocation, Option<CurrencyId>>
	for BifrostCurrencyIdConvert<T, H>
{
	fn convert(location: MultiLocation) -> Option<CurrencyId> {
		use CurrencyId::{Native, Stable, Token, VSToken};
		use TokenSymbol::*;

		if location == MultiLocation::parent() {
			return Some(Token(KSM));
		}
		match location {
			MultiLocation { parents, interior } if parents == 1 => match interior {
				X2(Parachain(id), GeneralKey(key)) if ParaId::from(id) == T::get() => {
					// decode the general key
					if let Ok(currency_id) = CurrencyId::decode(&mut &key[..]) {
						match currency_id {
							Native(TokenSymbol::ASG) |
							Native(TokenSymbol::BNC) |
							VSToken(TokenSymbol::KSM) => Some(currency_id),
							_ => None,
						}
					} else {
						None
					}
				},
				X2(Parachain(id), GeneralKey(key))
					if id == parachains::karura::ID || id == parachains::snowfork::ID =>
				{
					if key == parachains::karura::KAR_KEY.to_vec() {
						Some(Token(TokenSymbol::KAR))
					} else if key == parachains::karura::KUSD_KEY.to_vec() {
						Some(Stable(TokenSymbol::KUSD))
					} else if key == parachains::snowfork::AssetId::ETH.encode() {
						Some(Token(TokenSymbol::ETH))
					} else if key == parachains::snowfork::AssetId::Token(H::get()).encode() {
						Some(Token(TokenSymbol::VETH))
					} else {
						None
					}
				}
				_ => None,
			},
			_ => None,
		}
	}
}
impl<T: Get<ParaId>, H: Get<H160>> Convert<MultiAsset, Option<CurrencyId>>
	for BifrostCurrencyIdConvert<T, H>
{
	fn convert(asset: MultiAsset) -> Option<CurrencyId> {
		if let MultiAsset { id: Concrete(location), fun: Fungible(_) } = asset {
			Self::convert(location)
		} else {
			None
		}
	}
}

pub struct BifrostAccountIdToMultiLocation;
impl Convert<AccountId, MultiLocation> for BifrostAccountIdToMultiLocation {
	fn convert(account: AccountId) -> MultiLocation {
		X1(AccountId32 { network: NetworkId::Any, id: account.into() }).into()
	}
}
