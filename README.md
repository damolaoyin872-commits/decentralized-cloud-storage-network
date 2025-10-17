# Decentralized Cloud Storage Network

A peer-to-peer cloud storage system where users can rent unused hard drive space to others while earning cryptocurrency. This project implements smart contracts for a decentralized storage marketplace using Clarity on the Stacks blockchain.

## Overview

This system provides:
- Decentralized storage marketplace connecting storage providers with users
- File encryption, sharding, and distribution across multiple nodes
- Data redundancy and recovery mechanisms
- Fair pricing based on supply and demand dynamics
- Provider reliability tracking and incentives

## Key Components

### Storage Marketplace Coordinator
Manages the marketplace of storage providers and users, handling:
- Provider registration and reputation management
- Dynamic pricing based on supply and demand
- Payment processing and distribution
- Service level agreements

### Data Redundancy Manager
Ensures data integrity and availability through:
- File encryption and sharding
- Distribution of data across multiple storage nodes
- Erasure coding for data recovery
- Integrity verification through challenges

## Benefits

- **Cost-effective**: More affordable than centralized cloud providers
- **Censorship-resistant**: No single point of control
- **Privacy-focused**: Files are encrypted and sharded
- **Reliable**: Redundant storage with recovery mechanisms
- **Incentivized**: Storage providers earn cryptocurrency

## Smart Contracts

The project consists of two main smart contracts:
- `storage-marketplace-coordinator.clar`: Handles marketplace operations
- `data-redundancy-manager.clar`: Manages data integrity and redundancy

## Getting Started

This project uses Clarinet, a Clarity development tool for the Stacks blockchain.

```bash
# Run tests
clarinet test

# Check contract syntax
clarinet check

# Deploy contracts (on testnet/mainnet)
clarinet publish
```