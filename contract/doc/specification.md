# BLND on Tezos





This documentation describes a smart contract which implements FA1.2 interface and network bridge.

## Table of contents

- [Storage](#storage)
  - [Storage](#storage-Storage)
- [Entrypoints](#entrypoints)
  - [transfer](#entrypoints-transfer)
  - [approve](#entrypoints-approve)
  - [approveCAS](#entrypoints-approveCAS)
  - [getAllowance](#entrypoints-getAllowance)
  - [getBalance](#entrypoints-getBalance)
  - [getTotalSupply](#entrypoints-getTotalSupply)
  - [setPause](#entrypoints-setPause)
  - [setAdministrator](#entrypoints-setAdministrator)
  - [getAdministrator](#entrypoints-getAdministrator)
  - [mint](#entrypoints-mint)
  - [burn](#entrypoints-burn)
  - [lock](#entrypoints-lock)
  - [revealSecretHash](#entrypoints-revealSecretHash)

**[Definitions](#definitions)**

- [Types](#types)
  - [()](#types-lparenrparen)
  - [(a, b)](#types-lparenacomma-brparen)
  - [(a, b, c)](#types-lparenacomma-bcomma-crparen)
  - [Address (no entrypoint)](#types-Address-lparenno-entrypointrparen)
  - [BigMap](#types-BigMap)
  - [Bool](#types-Bool)
  - [BridgeStorage](#types-BridgeStorage)
  - [ByteString](#types-ByteString)
  - [Contract](#types-Contract)
  - [Integer](#types-Integer)
  - [LockParams](#types-LockParams)
  - [ManagedLedgerStorage](#types-ManagedLedgerStorage)
  - [Maybe](#types-Maybe)
  - [Named entry](#types-Named-entry)
  - [Natural](#types-Natural)
  - [Outcome](#types-Outcome)
  - [RevealSecretHashParams](#types-RevealSecretHashParams)
  - [Swap](#types-Swap)
  - [SwapId](#types-SwapId)
  - [Text](#types-Text)
  - [Timestamp](#types-Timestamp)
  - [View](#types-View)
- [Errors](#errors)
  - [AllowanceMismatch](#errors-AllowanceMismatch)
  - [InternalError](#errors-InternalError)
  - [NotEnoughAllowance](#errors-NotEnoughAllowance)
  - [NotEnoughBalance](#errors-NotEnoughBalance)
  - [SecreteHashIsAlreadySet](#errors-SecreteHashIsAlreadySet)
  - [SenderIsNotAdmin](#errors-SenderIsNotAdmin)
  - [SenderIsNotTheInitiator](#errors-SenderIsNotTheInitiator)
  - [SwapLockAlreadyExists](#errors-SwapLockAlreadyExists)
  - [SwapLockDoesNotExists](#errors-SwapLockDoesNotExists)
  - [TokenOperationsArePaused](#errors-TokenOperationsArePaused)
  - [UnsafeAllowanceChange](#errors-UnsafeAllowanceChange)



## Storage

<a name="storage-Storage"></a>

---

### `Storage`

Storage of the contract. It is splitted on two parts, one for `Token` storage and one for `Bridge` storage.

**Structure:** 
  * ***token*** :[`ManagedLedgerStorage`](#types-ManagedLedgerStorage)    
Managed ledger connected storage.
  * ***bridge*** :[`BridgeStorage`](#types-BridgeStorage)    
Bridge connected storage.

**Final Michelson representation:** `pair (pair (pair (big_map address nat) (big_map (pair address address) nat)) (pair address (pair bool nat))) (pair (big_map bytes (pair (pair address address) (pair nat timestamp))) (big_map bytes (or unit (or bytes bytes))))`



## Entrypoints

<a name="entrypoints-transfer"></a>

---

### `transfer`

Transfers tokens between two given accounts.

This entrypoint serves multiple purposes:
* When called with `"from"` account equal to the transaction sender, we assume that
the user transfers their own money and this does not require approval.
* Otherwise, the transaction sender must be previously authorized to transfer at least the requested number of tokens from the `"from"` account using the `approve` entrypoint.
In this case current number of tokens that sender is allowed to withdraw from the `"from"` address is decreased by the number of transferred tokens.



**Argument:** 
  + **In Haskell:** (***from*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***to*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :from) (pair (address :to) (nat :value)))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0)`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `transfer` entrypoint passing the constructed argument.
</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.

* [`NotEnoughAllowance`](#errors-NotEnoughAllowance) — Not enough funds allowance to perform the operation.

* [`NotEnoughBalance`](#errors-NotEnoughBalance) — Not enough funds to perform the operation.



<a name="entrypoints-approve"></a>

---

### `approve`

When called with `(address :spender, nat :value)`
parameters allows `spender` account to withdraw from the sender, multiple times,
up to the `value` amount.
Each call of `transfer` entrypoint decreases the allowance amount on the transferred amount of
tokens, unless `transfer` is called with `from` account equal to sender, in which case allowance
is always ignored.
In other terms self-approval, where 'from` is equal to sender, is redundant and will never be consumed by a 'transfer'.

If this entrypoint is called again, it overwrites the current allowance with `value`.

**DISCLAIMER**: this suffers from an [attack vector](https://docs.google.com/document/d/1YLPtQxZu1UAvO9cZ1O2RPXBbT0mooh4DYKjA_jp-RLM),
that is a known issue of `ERC20` and, as a consequence, of `FA1.2` (which is
based on it).
It is not safe to change the approval from a non-zero value to a non-zero value.
This is the reason why performing such a change directly is not allowed by the contract.
However this is not enough on its own, a token holder that intends to
safely change the allowance for `X` to `K` token must:
1. read the current allowance `M` for `X` from the latest transaction `S`.
2. send a transaction `T` that sets the allowance to `0`.
3. wait for the blockchain to confirm that `T` is included.
4. scan all transactions between `S` and `T`.
5. calculate the allowance `N <= M` spent by `X` in those transactions.
6. set the allowance to `K - N` iff `N < K`.


**Argument:** 
  + **In Haskell:** (***spender*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :spender) (nat :value))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `approve` entrypoint passing the constructed argument.
</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.

* [`UnsafeAllowanceChange`](#errors-UnsafeAllowanceChange) — Allowance change from non-zero value to non-zero value is performed.



<a name="entrypoints-approveCAS"></a>

---

### `approveCAS`

Compare the expected allowance value with the actual one and set a new one if they match.

If the current amount of sender's tokens `spender` is allowed to spend is **not** equal to the `expected` value, this function fails with `allowanceMismatch` error.
Otherwise it behaves as `approve` and does not prohibit changing allowance from non-zero to non-zero.


**Argument:** 
  + **In Haskell:** (***spender*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***value*** : [`Natural`](#types-Natural), ***expected*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :spender) (pair (nat :value) (nat :expected)))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair 0 0)`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `approveCAS` entrypoint passing the constructed argument.
</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.

* [`AllowanceMismatch`](#errors-AllowanceMismatch) — Expected allowance does not match the actual one



<a name="entrypoints-getAllowance"></a>

---

### `getAllowance`

Returns the approval value between two given addresses.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) (***owner*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***spender*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair (pair %viewParam (address :owner) (address :spender)) (contract %viewCallbackTo nat))`
    + **Example:** <span id="example-id">`Pair (Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB") "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getAllowance` entrypoint passing the constructed argument.
</details>
<p>





<a name="entrypoints-getBalance"></a>

---

### `getBalance`

Returns the balance of the address in the ledger.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) (***owner*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair (address :owner %viewParam) (contract %viewCallbackTo nat))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getBalance` entrypoint passing the constructed argument.
</details>
<p>





<a name="entrypoints-getTotalSupply"></a>

---

### `getTotalSupply`

Returns total number of tokens.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair (unit %viewParam) (contract %viewCallbackTo nat))`
    + **Example:** <span id="example-id">`Pair Unit "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getTotalSupply` entrypoint passing the constructed argument.
</details>
<p>





<a name="entrypoints-setPause"></a>

---

### `setPause`

This entrypoint pauses operations when the parameter is `True`,
and resumes them when the parameter is `False`. During the pause,
no contract can perform `transfer` or `approval` operations.


**Argument:** 
  + **In Haskell:** [`Bool`](#types-Bool)
  + **In Michelson:** `bool`
    + **Example:** <span id="example-id">`True`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `setPause` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`SenderIsNotAdmin`](#errors-SenderIsNotAdmin) — Entrypoint executed not by its administrator.



<a name="entrypoints-setAdministrator"></a>

---

### `setAdministrator`

Change the current administrator.

**Argument:** 
  + **In Haskell:** [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  + **In Michelson:** `address`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `setAdministrator` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`SenderIsNotAdmin`](#errors-SenderIsNotAdmin) — Entrypoint executed not by its administrator.



<a name="entrypoints-getAdministrator"></a>

---

### `getAdministrator`

This view returns the current administrator.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  + **In Michelson:** `(pair (unit %viewParam) (contract %viewCallbackTo address))`
    + **Example:** <span id="example-id">`Pair Unit "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getAdministrator` entrypoint passing the constructed argument.
</details>
<p>





<a name="entrypoints-mint"></a>

---

### `mint`

Produces tokens on the account associated with the given address.

**Argument:** 
  + **In Haskell:** (***to*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :to) (nat :value))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `mint` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`SenderIsNotAdmin`](#errors-SenderIsNotAdmin) — Entrypoint executed not by its administrator.



<a name="entrypoints-burn"></a>

---

### `burn`

Destroys the given amount of tokens on the account associated with the given address.

**Argument:** 
  + **In Haskell:** (***from*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :from) (nat :value))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `burn` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`SenderIsNotAdmin`](#errors-SenderIsNotAdmin) — Entrypoint executed not by its administrator.

* [`NotEnoughBalance`](#errors-NotEnoughBalance) — Not enough funds to perform the operation.



<a name="entrypoints-lock"></a>

---

### `lock`

**Argument:** 
  + **In Haskell:** [`LockParams`](#types-LockParams)
  + **In Michelson:** `(pair (pair (bytes %lpId) (address %lpTo)) (pair (nat %lpAmount) (pair (timestamp %lpReleaseTime) (option %lpSecretHash bytes))))`
    + **Example:** <span id="example-id">`Pair (Pair 0x0a "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB") (Pair 0 (Pair "2019-07-26T12:09:12Z" (Some 0x0a)))`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `lock` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`SwapLockAlreadyExists`](#errors-SwapLockAlreadyExists) — Lock with this id already exists

* [`NotEnoughBalance`](#errors-NotEnoughBalance) — Not enough funds to perform the operation.



<a name="entrypoints-revealSecretHash"></a>

---

### `revealSecretHash`

**Argument:** 
  + **In Haskell:** [`RevealSecretHashParams`](#types-RevealSecretHashParams)
  + **In Michelson:** `(pair (bytes %rshpId) (bytes %rshpSecreteHash))`
    + **Example:** <span id="example-id">`Pair 0x0a 0x0a`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `revealSecretHash` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`SwapLockDoesNotExists`](#errors-SwapLockDoesNotExists) — Lock with this id does not exists

* [`SenderIsNotTheInitiator`](#errors-SenderIsNotTheInitiator) — Sender is not the initiator of this swap

* [`SecreteHashIsAlreadySet`](#errors-SecreteHashIsAlreadySet) — Secrete hash is already set for swap with certain id







# Definitions

## Types

<a name="types-lparenrparen"></a>

---

### `()`

Unit primitive.

**Structure:** ()

**Final Michelson representation:** `unit`



<a name="types-lparenacomma-brparen"></a>

---

### `(a, b)`

Pair primitive.

**Final Michelson representation (example):** `(Integer,Natural)` = `pair int nat`



<a name="types-lparenacomma-bcomma-crparen"></a>

---

### `(a, b, c)`

Tuple of size 3.

**Final Michelson representation (example):** `(Integer,Natural,MText)` = `pair int (pair nat string)`



<a name="types-Address-lparenno-entrypointrparen"></a>

---

### `Address (no entrypoint)`

This is similar to Michelson Address, but does not retain entrypoint name if it refers to a contract.

**Final Michelson representation:** `address`



<a name="types-BigMap"></a>

---

### `BigMap`

BigMap primitive.

**Final Michelson representation (example):** `BigMap Integer Natural` = `big_map int nat`



<a name="types-Bool"></a>

---

### `Bool`

Bool primitive.

**Final Michelson representation:** `bool`



<a name="types-BridgeStorage"></a>

---

### `BridgeStorage`

Bridge storage.

**Structure:** 
  * ***swaps*** :[`BigMap`](#types-BigMap) [`SwapId`](#types-SwapId) [`Swap`](#types-Swap)    
Container with all swaps.
  * ***outcomes*** :[`BigMap`](#types-BigMap) [`SwapId`](#types-SwapId) [`Outcome`](#types-Outcome)    
Container with results of each swap.

**Final Michelson representation:** `pair (big_map bytes (pair (pair address address) (pair nat timestamp))) (big_map bytes (or unit (or bytes bytes)))`



<a name="types-ByteString"></a>

---

### `ByteString`

Bytes primitive.

**Final Michelson representation:** `bytes`



<a name="types-Contract"></a>

---

### `Contract`

Contract primitive with given type of parameter.

**Final Michelson representation (example):** `ContractRef Integer` = `contract int`



<a name="types-Integer"></a>

---

### `Integer`

Signed number.

**Final Michelson representation:** `int`



<a name="types-LockParams"></a>

---

### `LockParams`

Lock entrypoint params.

**Structure:** 
  * ***id*** :[`SwapId`](#types-SwapId)    
Swap id.
  * ***to*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)    
Address of swap reciever.
  * ***amount*** :[`Natural`](#types-Natural)    
Number of tokens in swap.
  * ***releaseTime*** :[`Timestamp`](#types-Timestamp)    
Time for swap process.
  * ***secretHash*** :[`Maybe`](#types-Maybe) [`ByteString`](#types-ByteString)    
Hash of the secret.

**Final Michelson representation:** `pair (pair bytes address) (pair nat (pair timestamp (option bytes)))`



<a name="types-ManagedLedgerStorage"></a>

---

### `ManagedLedgerStorage`

Managed ledger storage.

**Structure:** 
  * ***ledger*** :[`BigMap`](#types-BigMap) [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen) (***balance*** : [`Natural`](#types-Natural))    
Ledger which map addresses to its token balance.
  * ***approvals*** :[`BigMap`](#types-BigMap) (***owner*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***spender*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)) [`Natural`](#types-Natural)    
Map from approvals to its value.
  * ***admin*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)    
Addres of the admin of the token.
  * ***paused*** :[`Bool`](#types-Bool)    
Pause of managed ledger.
  * ***totalSupply*** :[`Natural`](#types-Natural)    
Total amount of tokens.

**Final Michelson representation:** `pair (pair (big_map address nat) (big_map (pair address address) nat)) (pair address (pair bool nat))`



<a name="types-Maybe"></a>

---

### `Maybe`

Option primitive.

**Final Michelson representation (example):** `Maybe Integer` = `option int`



<a name="types-Named-entry"></a>

---

### `Named entry`

Some entries have names for clarity.

In resulting Michelson names may be mapped to annotations.

**Final Michelson representation (example):** `number: Integer` = `int`



<a name="types-Natural"></a>

---

### `Natural`

Unsigned number.

**Final Michelson representation:** `nat`



<a name="types-Outcome"></a>

---

### `Outcome`

Outcome storage fields.

**Structure:** *one of* 
+ **Refunded**(): Swap was refunded
+ **HashRevealed**
[`ByteString`](#types-ByteString): Secret hash was revealed
+ **SecretRevealed**
[`ByteString`](#types-ByteString): Secret was revealed


**Final Michelson representation:** `or unit (or bytes bytes)`



<a name="types-RevealSecretHashParams"></a>

---

### `RevealSecretHashParams`

RevealSecretHash entrypoint params.

**Structure:** 
  * ***id*** :[`SwapId`](#types-SwapId)    
Swap id.
  * ***secreteHash*** :[`ByteString`](#types-ByteString)    
Hash of the secret.

**Final Michelson representation:** `pair bytes bytes`



<a name="types-Swap"></a>

---

### `Swap`

Swap information.

**Structure:** 
  * ***from*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)    
Address of swap initiator.
  * ***to*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)    
Address of swap reciever.
  * ***amount*** :[`Natural`](#types-Natural)    
Number of tokens in swap.
  * ***releaseTime*** :[`Timestamp`](#types-Timestamp)    
Time for swap process.

**Final Michelson representation:** `pair (pair address address) (pair nat timestamp)`



<a name="types-SwapId"></a>

---

### `SwapId`

Id of the swap.

**Structure:** 
[`ByteString`](#types-ByteString)

**Final Michelson representation:** `bytes`



<a name="types-Text"></a>

---

### `Text`

Michelson string.

This has to contain only ASCII characters with codes from [32; 126] range; additionally, newline feed character is allowed.

**Final Michelson representation:** `string`



<a name="types-Timestamp"></a>

---

### `Timestamp`

Timestamp primitive.

**Final Michelson representation:** `timestamp`



<a name="types-View"></a>

---

### `View`

`View a r` accepts an argument of type `a` and callback contract which accepts `r` and returns result via calling that contract.
Read more in [A1 conventions document](https://gitlab.com/tzip/tzip/-/blob/c42e3f0f5e73669e84e615d69bee73281572eb0a/proposals/tzip-4/tzip-4.md#view-entrypoints).

**Structure (example):** `View () Integer` = 
[`()`](#types-lparenrparen)
[`ContractRef`](#types-Contract) [`Integer`](#types-Integer)

**Final Michelson representation (example):** `View () Integer` = `pair unit (contract int)`



## Errors

Our contract implies the possibility of error scenarios, this section enlists
all values which the contract can produce via calling `FAILWITH` instruction
on them. In case of error, no changes to contract state will be applied.

Each entrypoint also contains a list of errors which can be raised during its
execution; only for no-throw entrypoints this list will be omitted.
Errors in these lists are placed in the order in which the corresponding
properties are checked unless the opposite is specified. I.e., if for a
given entrypoint call two different errors may take place, the one which
appears in the list first will be thrown.

Most of the errors are represented according to the same
`(error tag, error argument)` pattern. See the list of errors below
for details.

We distinquish several error classes:
+ **Action exception**: given action cannot be performed with
  regard to the current contract state.

  Examples: "insufficient balance", "wallet does not exist".

  If you are implementing a middleware, such errors should be propagated to
  the client.

+ **Bad argument**: invalid argument supplied to the entrypoint.

  Examples: entrypoint accepts a natural number from `0-3` range, and you
  supply `5`.

  If you are implementing a middleware, you should care about not letting
  such errors happen.

+ **Internal**: contract-internal error.

  In ideal case, such errors should not take place, but still, make sure
  that you are ready to handle them. They can signal either invalid contract
  deployment or a bug in contract implementation.

  If an internal error is thrown, please report it to the author of this contract.


<a name="errors-AllowanceMismatch"></a>

---

### `AllowanceMismatch`

**Class:** Bad argument

**Fires if:** Expected allowance does not match the actual one

**Representation:** `("AllowanceMismatch", <error argument>)`.

Provided error argument will be of type (***actual*** : [`Natural`](#types-Natural), ***expected*** : [`Natural`](#types-Natural)) and stand for `(actual allowance, expected allowance)` pair.

<a name="errors-InternalError"></a>

---

### `InternalError`

**Class:** Internal

**Fires if:** Some internal error occured.

**Representation:** Textual error message, see [`Text`](#types-Text).

<a name="errors-NotEnoughAllowance"></a>

---

### `NotEnoughAllowance`

**Class:** Action exception

**Fires if:** Not enough funds allowance to perform the operation.

**Representation:** `("NotEnoughAllowance", <error argument>)`.

Provided error argument will be of type (***required*** : [`Natural`](#types-Natural), ***present*** : [`Natural`](#types-Natural)).

<a name="errors-NotEnoughBalance"></a>

---

### `NotEnoughBalance`

**Class:** Action exception

**Fires if:** Not enough funds to perform the operation.

**Representation:** `("NotEnoughBalance", <error argument>)`.

Provided error argument will be of type (***required*** : [`Natural`](#types-Natural), ***present*** : [`Natural`](#types-Natural)).

<a name="errors-SecreteHashIsAlreadySet"></a>

---

### `SecreteHashIsAlreadySet`

**Class:** Action exception

**Fires if:** Secrete hash is already set for swap with certain id

**Representation:** `("SecreteHashIsAlreadySet", <error argument>)`.

Provided error argument will be of type [`SwapId`](#types-SwapId) and stand for swap id.

<a name="errors-SenderIsNotAdmin"></a>

---

### `SenderIsNotAdmin`

**Class:** Action exception

**Fires if:** Entrypoint executed not by its administrator.

**Representation:** `("SenderIsNotAdmin", ())`.

<a name="errors-SenderIsNotTheInitiator"></a>

---

### `SenderIsNotTheInitiator`

**Class:** Action exception

**Fires if:** Sender is not the initiator of this swap

**Representation:** `("SenderIsNotTheInitiator", ())`.

<a name="errors-SwapLockAlreadyExists"></a>

---

### `SwapLockAlreadyExists`

**Class:** Action exception

**Fires if:** Lock with this id already exists

**Representation:** `("SwapLockAlreadyExists", <error argument>)`.

Provided error argument will be of type [`SwapId`](#types-SwapId) and stand for swap id.

<a name="errors-SwapLockDoesNotExists"></a>

---

### `SwapLockDoesNotExists`

**Class:** Action exception

**Fires if:** Lock with this id does not exists

**Representation:** `("SwapLockDoesNotExists", <error argument>)`.

Provided error argument will be of type [`SwapId`](#types-SwapId) and stand for swap id.

<a name="errors-TokenOperationsArePaused"></a>

---

### `TokenOperationsArePaused`

**Class:** Action exception

**Fires if:** Token functionality (`transfer` and similar entrypoints) is suspended.

**Representation:** `("TokenOperationsArePaused", ())`.

<a name="errors-UnsafeAllowanceChange"></a>

---

### `UnsafeAllowanceChange`

**Class:** Action exception

**Fires if:** Allowance change from non-zero value to non-zero value is performed. This contract does not allow such an update, see the [corresponding attack vector](https://docs.google.com/document/d/1YLPtQxZu1UAvO9cZ1O2RPXBbT0mooh4DYKjA_jp-RLM) for explanation.

**Representation:** `("UnsafeAllowanceChange", <error argument>)`.

Provided error argument will be of type [`Natural`](#types-Natural) and stand for the previous value of approval.
