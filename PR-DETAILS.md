# Storage Network Smart Contracts

This pull request introduces two key smart contracts for the decentralized cloud storage network:

## Storage Marketplace Coordinator

- Manages a marketplace connecting storage providers with users
- Handles provider registration with reputation system
- Implements dynamic pricing based on market conditions
- Processes subscription creation and payment flows
- Supports geographic distribution of storage

## Data Redundancy Manager

- Handles file encryption, sharding and distribution
- Implements data verification challenges for providers
- Ensures data integrity through merkle proofs
- Supports automatic recovery when nodes fail
- Manages shard replication across the network

These contracts form the foundation of a peer-to-peer storage network where users can rent unused hard drive space from providers while maintaining security through encryption and redundancy.

## Testing

Both contracts have been verified with `clarinet check` for syntax and type checking.