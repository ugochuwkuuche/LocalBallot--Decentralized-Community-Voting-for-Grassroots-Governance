import { describe, it, expect } from 'vitest';
import { Cl } from '@stacks/transactions';

const accounts = simnet.getAccounts();
const deployer = accounts.get('deployer')!;
const alice = accounts.get('wallet_1')!;
const bob = accounts.get('wallet_2')!;

describe('Community Events Contract', () => {
  it('should create a new event successfully', () => {
    const result = simnet.callPublicFn(
      'Community-Events',
      'create-event',
      [
        Cl.stringAscii('Town Hall Meeting'),
        Cl.stringAscii('Monthly community meeting to discuss local issues'),
        Cl.stringAscii('meeting'),
        Cl.stringAscii('Community Center Main Room'),
        Cl.uint(1000), // start-date
        Cl.uint(1100), // end-date
        Cl.uint(50),   // max-capacity
        Cl.uint(950),  // registration-deadline
        Cl.bool(true)  // is-public
      ],
      alice
    );

    expect(result.result).toBeOk(Cl.uint(1));
  });

  it('should get event count', () => {
    const result = simnet.callReadOnlyFn(
      'Community-Events',
      'get-event-count',
      [],
      alice
    );

    expect(result.result).toStrictEqual(Cl.uint(0));
  });

  it('should fail to create event with invalid dates', () => {
    const result = simnet.callPublicFn(
      'Community-Events',
      'create-event',
      [
        Cl.stringAscii('Invalid Event'),
        Cl.stringAscii('Event with invalid dates'),
        Cl.stringAscii('meeting'),
        Cl.stringAscii('Community Center'),
        Cl.uint(3000), // start-date
        Cl.uint(3100), // end-date
        Cl.uint(50),
        Cl.uint(3200), // registration-deadline AFTER start-date (invalid)
        Cl.bool(true)
      ],
      alice
    );

    expect(result.result).toBeErr(Cl.uint(109)); // ERR-INVALID-DATE
  });
});
