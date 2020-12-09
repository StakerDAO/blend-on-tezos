# Bridge specification

**Bridge** contract allows us to swap BLND tokens between two nets: Tezos and Ethereum. For this 
opportunity there are two contracts on each net, which together implement swap protocol
based on secret hash.

To produce this swap we need to set up useful environment and test swap protocol by hands.

## Environment setup

### Tezos

#### Tezos client

First of all you need to set up a Tezos client.

##### On Linux

* Download `tezos-client` and `tezos-node` binaries from 
[`tezos-packaging`](https://github.com/serokell/tezos-packaging);
* Run `chmod +x` on these files;
* Place them somewhere in your `$PATH`, e.g. `~/.local/bin`.

##### On macOS
* `brew tap tqtezos/homebrew-tq https://github.com/tqtezos/homebrew-tq.git`
* `brew install tezos`

After that `tezos-client` and `tezos-node` command should be available in your `$PATH`.

#### Identity

The next step will be the identification ourselves in the network. First of all lets generate an 
identity using `tezos-node identity generate` command. It will take a short amount of time. 
After the end, you will be able to see your identity here `~/.tezos-node/identity.json`.

#### Account

Before we will start to use the test net we need to set up the node for `tezos-client`.
With `tezos-client -A <IP addr|host> -P <port number> config update` you can easily add it to
`tezos-client` config. For example, you can use **Serokell** node: 
`delphi.testnet.tezos.serokell.team:8732`

Now we can start to interact with Tezos test net, but we still have no account with money. So, lets 
get it:
* Navigate to https://faucet.tzalpha.net and download the file. This file will contains all
the required information for account activation.
* Activate account using 
`tezos-client activate account <account_alias> with <path_to_the_downloaded_file>` command
* After that you will be able to see your address in the net. Lets open 
[block explorer](https://tzkt.io/), choose `carthage` net and search for the address.

One more thing you need to do before swap test is to create the account with whom the swap will be 
produced:
* Generate new account `tezos-client gen keys bob`
* And transfer some money to him `tezos-client transfer 1000 from alice to bob`
* You will see error about burn cap and needed quantity to execute this operation.
* Add ` --burn-cap <burn_cap_number_from_error message>` to the previous command and try again.

You can easily see all addresses using `tezos-client list known addresses`

#### Originate the contract

After creating identity and accounts we can originate our bridge contract. You can find contract
code [here](../contract/morley-contract/contract.tz) with 
[storage](../contract/morley-contract/storage.tz) example. Lets originate this contract and add
previously created accounts to its storage (you alos may need to add a --burn-cap flag to this 
command):

```
tezos-client originate contract blndOnTezos transferring 1 from alice running <path_to_contract> \
  --init 'Pair (Pair (Pair {Elt <alice_addres> 1000; Elt <bob_address> 1000} { }) \
  (Pair <alice_addres> (Pair False 2000))) (Pair { } { })'
```

This command will originate bridge contract with alias `blndOnTezos`, two accounts holding 
1000 tokens and `alice` as administrator. Also in the command output you can see contract address 
and find it in the [block explorer](https://better-call.dev/).

Now you are ready to interact with Tezos part of `Bridge` contract.

### Ethereum

#### Build

First clone [Blend](https://github.com/StakerDAO/blend) repository, it contains all needed
things to build, deploy and interact with Bridge contract. To build it you need to have Node v14 and
install all with `npm install`. Then build and compile contracts with `npm run build` and 
`npx oz compile`.

#### Account

One more thing you need to do before contract deploy is Ethereum account with money on testnet.
To get it create `.env` file and fill it as in `.env.example`. Infura data could be taken from
[infura](http://infura.io/) service after registration and new project creation. And mnemonic with 
12/24 length generated [here](https://iancoleman.io/bip39/).

After that run `npx oz accounts`. You will see all accounts with your mnemonic and the last step is
to ask money [here](https://goerli-faucet.slock.it/) for default account.

#### Originate contracts and prepare their storage

Now you have all needed things deploy contracts. The main difficult with Ethereum part is that for 
`Bridge` contract we also need to have `BlendToken` contract. So, we need to deploy it too.

So, first let's deploy `BlendToken` contract:
```
> ./bin/staker-blend deploy <Owner Address>
? Pick a network goerli
? Multisig threshold (1 <= N <= 1) 1
? Address of BLND initial holder <You can choose owner address or whatever you want>
? Total supply (in BLND tokens) <as much as you want>
? Distribution backend address <not important if you don't want to run blnd-app>
? Registry backend address <not important if you don't want to run blnd-app>
? USDC pool address <not important if you don't want to run blnd-app>
? Address of USDC token <not important if you don't want to run blnd-app>
```
__Attention__: with a high probability deploy will fail with `gas limit` error. In this case just
restart it. May required more then one restart. After successful deploy you will see your contract 
addresses in `goerli.json` file. If your deployment didn't happen successfully from the first time 
then remove all leading objects in every contract array and leave only last one.

Now we can deploy swap contract, which is simple:
```
./bin/staker-blend swap-deploy
? Pick a network goerli
? Address of BlendToken <address from goerli.json>
```

The last thing we need is `BlendTokens` on our acconts. To get them call `mint` entrypoint of 
`BlendToken` contract:
```
npx oz send-tx
? Pick a network goerli
? Pick an instance BlendToken at <BlendToken address from goerli>
? Select which function mint(beneficiary: address, amount: uint256)
? beneficiary: address: <first default address and then some seccond address>
? amount: uint256: <amount>
```

## Contract interaction

After you deployed contracts on both networks it is time to go through swap protocol and test them.
In fact contract interaction is very simple, so here will be just helpful notes and explanations of 
how to interact with them in command line.

* Each swap starts from secret and its hash. You can get them from 
  `./bin/staker-blend swap-generate`
* Ethereum lock timeout is hardcoded. So, to change it from one hour to your values you need to
change it in `./src/commands/swap/lock.ts` in `timeout` variable.
  
### Tezos

Tezos has very userfriendly interface to create commands. When you will open `Bridge` contract in
[block explorer](https://better-call.dev/) you will see `interact` tab. Here you can choose needed 
entrypoint, fill data and execute it using web extension, simulation or just copy-paste 
`tezos-client` arguments and execute from command line.

Also in block explorer you can contract storage and how each transaction change it.

### Ethereum

Ethereum doesn't have such block explorer, but we have useful commant line interface. It consists 
from three parts:
1. The first part is for sending transaction to contracts except of `Bridge`. So, it is not needed 
for swap testing. You can call it using `npx oz send-tx`.
2. The second part is for fetching data from contract storage. You can call it using
`npx oz call`.
3. And the third part is for calling `Bridge` contract entrypoints. Add methods you can find in 
`./bin/staker-blend --help`. THey have `swap` prefix.
