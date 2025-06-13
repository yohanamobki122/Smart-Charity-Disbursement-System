# Smart Charity Disbursement System

A blockchain-based charity fund management system that reduces corruption by implementing milestone-based fund release. The system ensures transparency and accountability in charity fund disbursement.

## Overview

The Smart Charity Disbursement System allows:

1. Creation of charity campaigns with predefined milestones
2. Transparent donation tracking
3. Independent milestone approval by authorized approvers
4. Milestone-based fund disbursement
5. Complete transparency of all transactions and approvals

## Contract Functions

### Administrative Functions

- `set-contract-owner`: Transfer contract ownership
- `add-approver`: Add an authorized milestone approver
- `remove-approver`: Remove an approver's authorization

### Charity Management

- `create-charity`: Create a new charity campaign with a specified number of milestones
- `add-milestone`: Define a milestone with description and percentage of funds to release
- `deactivate-charity`: Deactivate a charity campaign

### Donation Functions

- `donate-to-charity`: Donate STX to a specific charity

### Milestone Management

- `approve-milestone`: Authorized approvers can verify milestone completion
- `complete-milestone`: Charity creators can claim funds after milestone approval

### Read-Only Functions

- `get-charity`: View charity details
- `get-milestone`: View milestone details
- `get-donor-contribution`: Check donation amount from a specific donor
- `is-approver`: Check if an address is an authorized approver
- `get-owner`: Get the contract owner's address

## Usage Example

1. Contract owner adds approvers:
   ```
   (contract-call? .s-charity add-approver 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
   ```

2. Create a charity campaign:
   ```
   (contract-call? .s-charity create-charity "Clean Water Initiative" "Providing clean water to rural communities" u3)
   ```

3. Define milestones:
   ```
   (contract-call? .s-charity add-milestone u1 u1 "Purchase equipment" u30)
   (contract-call? .s-charity add-milestone u1 u2 "Install in first village" u30)
   (contract-call? .s-charity add-milestone u1 u3 "Complete project documentation" u40)
   ```

4. Donate to the charity:
   ```
   (contract-call? .s-charity donate-to-charity u1 u1000000)
   ```

5. Approver verifies milestone completion:
   ```
   (contract-call? .s-charity approve-milestone u1 u1)
   ```

6. Charity creator claims funds after approval:
   ```
   (contract-call? .s-charity complete-milestone u1 u1)
   ```

## Security Features

- Only authorized approvers can approve milestones
- Funds are released incrementally based on milestone completion
- Charity creators cannot approve their own milestones
- Contract owner has oversight capabilities
```
