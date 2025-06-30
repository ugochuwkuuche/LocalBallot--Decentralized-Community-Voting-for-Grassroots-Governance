import { Clarinet, Tx, Chain, Account, types } from '@stacks/transactions';

Clarinet.test({
  name: "Ensures users can join community and create proposals",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const user1 = accounts.get("wallet_1")!;

    let block = chain.mineBlock([
      Tx.contractCall("voting", "join-community", [], user1.address),
      Tx.contractCall("voting", "create-proposal",
        ["Test Proposal", "Description", types.uint(100), types.uint(5)],
        user1.address
      )
    ]);

    block.receipts[0].result.expectOk().expectBool(true);
    block.receipts[1].result.expectOk().expectUint(1);
  }
});

Clarinet.test({
  name: "Ensures voting works correctly",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const user1 = accounts.get("wallet_1")!;
    const user2 = accounts.get("wallet_2")!;

    let block = chain.mineBlock([
      Tx.contractCall("voting", "join-community", [], user1.address),
      Tx.contractCall("voting", "join-community", [], user2.address),
      Tx.contractCall("voting", "create-proposal",
        ["Test Proposal", "Description", types.uint(100), types.uint(2)],
        user1.address
      ),
      Tx.contractCall("voting", "cast-vote",
        [types.uint(1), types.bool(true)],
        user1.address
      ),
      Tx.contractCall("voting", "cast-vote",
        [types.uint(1), types.bool(false)],
        user2.address
      )
    ]);

    block.receipts.map((receipt: any) => receipt.result.expectOk());
  }
});
