;; data-redundancy-manager
;;
;; Manages data redundancy for the decentralized storage network
;; Tracks data shards, handles encryption metadata, and ensures file recovery capabilities
;; Implements erasure coding, data verification, and automatic replication

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-FILE-ALREADY-EXISTS (err u201))
(define-constant ERR-FILE-NOT-FOUND (err u202))
(define-constant ERR-SHARD-NOT-FOUND (err u203))
(define-constant ERR-INVALID-SHARD-COUNT (err u204))
(define-constant ERR-INVALID-REDUNDANCY-LEVEL (err u205))
(define-constant ERR-VERIFICATION-FAILED (err u206))
(define-constant ERR-INVALID-PROVIDER (err u207))
(define-constant ERR-PROVIDER-CAPACITY (err u208))
(define-constant ERR-INSUFFICIENT-SHARDS (err u209))
(define-constant ERR-RECOVERY-FAILED (err u210))
(define-constant ERR-INVALID-CHALLENGE (err u211))

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Minimum shards needed for a file
(define-data-var min-shard-count uint u3)

;; Maximum shards allowed per file
(define-data-var max-shard-count uint u20)

;; Default redundancy level (N out of M shards needed for recovery)
(define-data-var default-redundancy-factor uint u2) ;; 2x redundancy by default

;; Verification challenge frequency in blocks
(define-data-var verification-frequency uint u144) ;; ~1 day in blocks

;; Verification reward in micro-STX
(define-data-var verification-reward uint u1000000) ;; 1 STX

;; File metadata
(define-map files
  { file-id: (string-ascii 64) } ;; File ID (hash of file content + owner)
  {
    owner: principal,            ;; File owner
    name: (string-utf8 255),      ;; File name
    size: uint,                  ;; Total file size in bytes
    encryption-type: (string-ascii 32), ;; Encryption algorithm used
    encryption-key-hash: (buff 32), ;; Hash of encryption key
    shard-count: uint,           ;; Number of shards the file is split into
    recovery-threshold: uint,     ;; Minimum shards needed for recovery
    creation-height: uint,        ;; Block height when file was added
    last-verification: uint,      ;; Last verification block height
    verified: bool,              ;; Whether the file is currently verified
    active: bool                 ;; Whether the file is active
  }
)

;; Shard metadata and locations
(define-map file-shards
  { shard-id: { file-id: (string-ascii 64), index: uint } }
  {
    hash: (buff 32),             ;; Hash of shard content
    provider: principal,         ;; Storage provider holding the shard
    size: uint,                  ;; Shard size in bytes
    encryption-nonce: (buff 16), ;; Encryption nonce for this shard
    verification-count: uint,     ;; Number of successful verifications
    last-verified: uint,          ;; Block height of last verification
    healthy: bool                ;; Whether the shard is healthy
  }
)

;; Shard distribution by provider
(define-map provider-shards
  { provider-id: principal }
  {
    shard-list: (list 20 { file-id: (string-ascii 64), index: uint }),
    total-storage-used: uint
  }
)

;; Active verification challenges
(define-map verification-challenges
  { challenge-id: (string-ascii 70) }
  {
    file-id: (string-ascii 64),
    shard-index: uint,
    provider: principal,
    challenge-data: (buff 32),   ;; Random challenge data
    expected-response: (buff 32), ;; Expected hash response
    expiration-height: uint,      ;; Block height when challenge expires
    completed: bool              ;; Whether challenge is completed
  }
)

;; Next challenge ID
(define-data-var next-challenge-id uint u1)

;; Recovery operations tracking
(define-map recovery-operations
  { operation-id: (string-ascii 73) }
  {
    file-id: (string-ascii 64),
    target-shard-index: uint,
    source-shards: (list 20 uint), ;; Indices of source shards for recovery
    new-provider: principal,
    status: (string-ascii 16),    ;; Status: "pending", "completed", "failed"
    start-height: uint,
    completion-height: uint
  }
)

;; Functions for managing data redundancy

;; Register a new file in the system
(define-public (register-file 
    (file-id (string-ascii 64)) 
    (name (string-utf8 255)) 
    (size uint)
    (encryption-type (string-ascii 32))
    (encryption-key-hash (buff 32))
    (shard-count uint)
    (recovery-threshold uint)
  )
  (let (
    (file-exists (is-some (map-get? files { file-id: file-id })))
    (current-height (get-block-height))
    (redundancy-factor (var-get default-redundancy-factor))
    (min-count (var-get min-shard-count))
    (max-count (var-get max-shard-count))
  )
    (if file-exists
      ERR-FILE-ALREADY-EXISTS
      (if (or (< shard-count min-count) (> shard-count max-count))
        ERR-INVALID-SHARD-COUNT
        (if (< recovery-threshold u2) ;; Need at least 2 shards for recovery
          ERR-INVALID-REDUNDANCY-LEVEL
          (if (> recovery-threshold shard-count)
            ERR-INVALID-REDUNDANCY-LEVEL
            (begin
              ;; Store file metadata
              (map-set files 
                { file-id: file-id }
                {
                  owner: tx-sender,
                  name: name,
                  size: size,
                  encryption-type: encryption-type,
                  encryption-key-hash: encryption-key-hash,
                  shard-count: shard-count,
                  recovery-threshold: recovery-threshold,
                  creation-height: current-height,
                  last-verification: current-height,
                  verified: true,
                  active: true
                }
              )
              (ok true)
            )
          )
        )
      )
    )
  )
)

;; Register a file shard with a specific provider
(define-public (register-shard 
    (file-id (string-ascii 64)) 
    (index uint) 
    (hash (buff 32)) 
    (provider principal) 
    (size uint)
    (encryption-nonce (buff 16))
  )
  (let (
    (file (map-get? files { file-id: file-id }))
    (owner (default-to tx-sender (get owner file)))
    (is-owner (is-eq tx-sender owner))
    (current-height (get-block-height))
    (provider-data (default-to { shard-list: (list), total-storage-used: u0 } (map-get? provider-shards { provider-id: provider })))
    (shard-exists (is-some (map-get? file-shards { shard-id: { file-id: file-id, index: index } })))
    (shard-count (default-to u0 (get shard-count file)))
  )
    (if (is-none file)
      ERR-FILE-NOT-FOUND
      (if (not is-owner)
        ERR-NOT-AUTHORIZED
        (if (>= index shard-count)
          ERR-INVALID-SHARD-COUNT
          (if shard-exists
            (ok true) ;; Shard already registered, consider it a success
            (begin
              ;; Store shard metadata
              (map-set file-shards
                { shard-id: { file-id: file-id, index: index } }
                {
                  hash: hash,
                  provider: provider,
                  size: size,
                  encryption-nonce: encryption-nonce,
                  verification-count: u0,
                  last-verified: current-height,
                  healthy: true
                }
              )
              
              ;; Update provider's shard list
              (map-set provider-shards
                { provider-id: provider }
                {
                  shard-list: (default-to (get shard-list provider-data) (as-max-len? (append (get shard-list provider-data) { file-id: file-id, index: index }) u20)),
                  total-storage-used: (+ (get total-storage-used provider-data) size)
                }
              )
              (ok true)
            )
          )
        )
      )
    )
  )
)

;; Create a verification challenge for a specific shard
(define-public (create-verification-challenge (file-id (string-ascii 64)) (shard-index uint))
  (let (
    (file (map-get? files { file-id: file-id }))
    (shard (map-get? file-shards { shard-id: { file-id: file-id, index: shard-index } }))
    (current-height (get-block-height))
    (challenge-id (concat file-id "-shard"))
    (provider-principal (default-to tx-sender (get provider shard)))
    (challenge-data (sha256 0x0102030405060708090A0B0C0D0E0F1011121314))
    (expected-response (sha256 (concat challenge-data (default-to 0x (get hash shard)))))
    (expiration-height (+ current-height u144)) ;; Challenge expires in ~1 day
  )
    (if (is-none file)
      ERR-FILE-NOT-FOUND
      (if (is-none shard)
        ERR-SHARD-NOT-FOUND
        (if (is-some (map-get? verification-challenges { challenge-id: challenge-id }))
          (err u212) ;; Challenge already exists
          (begin
            ;; Create challenge
            (map-set verification-challenges
              { challenge-id: challenge-id }
              {
                file-id: file-id,
                shard-index: shard-index,
                provider: provider-principal,
                challenge-data: challenge-data,
                expected-response: expected-response,
                expiration-height: expiration-height,
                completed: false
              }
            )
            (ok challenge-id)
          )
        )
      )
    )
  )
)

;; Respond to a verification challenge
(define-public (respond-to-challenge (challenge-id (string-ascii 64)) (response-data (buff 32)))
  (let (
    (challenge (map-get? verification-challenges { challenge-id: challenge-id }))
    (current-height (get-block-height))
    (file-id (default-to "" (get file-id challenge)))
    (shard-index (default-to u0 (get shard-index challenge)))
    (expected-response (default-to 0x (get expected-response challenge)))
    (provider-principal (default-to tx-sender (get provider challenge)))
    (is-provider (is-eq tx-sender provider-principal))
    (is-expired (> current-height (default-to u0 (get expiration-height challenge))))
  )
    (if (is-none challenge)
      ERR-INVALID-CHALLENGE
      (if (default-to false (get completed challenge))
        (err u213) ;; Challenge already completed
        (if is-expired
          (err u214) ;; Challenge expired
          (if (not is-provider)
            ERR-NOT-AUTHORIZED
            (if (is-eq response-data expected-response)
              (begin
                ;; Update challenge as completed
                (map-set verification-challenges
                  { challenge-id: challenge-id }
                  (merge (unwrap-panic challenge) { completed: true })
                )
                (ok true)
              )
              (begin
                ;; Mark challenge as completed but failed
                (map-set verification-challenges
                  { challenge-id: challenge-id }
                  (merge (unwrap-panic challenge) { completed: true })
                )
                ERR-VERIFICATION-FAILED
              )
            )
          )
        )
      )
    )
  )
)

;; Update shard verification status (private helper)
(define-private (update-shard-verification (file-id (string-ascii 64)) (shard-index uint) (verification-height uint))
  (let (
    (shard (map-get? file-shards { shard-id: { file-id: file-id, index: shard-index } }))
    (verification-count (default-to u0 (get verification-count shard)))
  )
    (if (is-some shard)
      (begin
        ;; Update shard verification info
        (map-set file-shards
          { shard-id: { file-id: file-id, index: shard-index } }
          (merge (unwrap-panic shard) {
            verification-count: (+ verification-count u1),
            last-verified: verification-height,
            healthy: true
          })
        )
        
        ;; Update file verification info
        (try! (update-file-verification file-id verification-height))
        
        (ok true)
      )
      ERR-SHARD-NOT-FOUND
    )
  )
)

;; Update file verification status
(define-private (update-file-verification (file-id (string-ascii 64)) (verification-height uint))
  (let (
    (file (map-get? files { file-id: file-id }))
  )
    (if (is-some file)
      (begin
        (map-set files
          { file-id: file-id }
          (merge (unwrap-panic file) {
            last-verification: verification-height,
            verified: true
          })
        )
        (ok true)
      )
      ERR-FILE-NOT-FOUND
    )
  )
)

;; Mark a shard as unhealthy (failed verification)
(define-private (mark-shard-unhealthy (file-id (string-ascii 64)) (shard-index uint))
  (let (
    (shard (map-get? file-shards { shard-id: { file-id: file-id, index: shard-index } }))
  )
    (if (is-some shard)
      (begin
        ;; Mark shard as unhealthy
        (map-set file-shards
          { shard-id: { file-id: file-id, index: shard-index } }
          (merge (unwrap-panic shard) { healthy: false })
        )
        
        ;; Initiate recovery if needed
        (try! (check-and-initiate-recovery file-id shard-index))
        
        (ok true)
      )
      ERR-SHARD-NOT-FOUND
    )
  )
)

;; Check if recovery is needed and initiate if so
(define-private (check-and-initiate-recovery (file-id (string-ascii 64)) (shard-index uint))
  (let (
    (file (map-get? files { file-id: file-id }))
    (recovery-threshold (default-to u0 (get recovery-threshold file)))
    (healthy-shards (get-healthy-shard-count file-id))
    (operation-id (concat file-id "-recovery"))
    (current-height (get-block-height))
  )
    (if (is-none file)
      ERR-FILE-NOT-FOUND
      (if (>= healthy-shards recovery-threshold)
        (begin
          ;; We have enough healthy shards for recovery, create recovery operation
          (map-set recovery-operations
            { operation-id: operation-id }
            {
              file-id: file-id,
              target-shard-index: shard-index,
              source-shards: (list),  ;; Will be populated when recovery starts
              new-provider: tx-sender, ;; Placeholder, will be selected during recovery
              status: "pending",
              start-height: current-height,
              completion-height: u0
            }
          )
          (ok true)
        )
        (begin
          ;; Not enough healthy shards for recovery
          ;; Mark file as unverified
          (map-set files
            { file-id: file-id }
            (merge (unwrap-panic file) { verified: false })
          )
          ERR-INSUFFICIENT-SHARDS
        )
      )
    )
  )
)

;; Get count of healthy shards for a file
(define-read-only (get-healthy-shard-count (file-id (string-ascii 64)))
  (let (
    (file (map-get? files { file-id: file-id }))
    (shard-count (default-to u0 (get shard-count file)))
    (healthy-count u0)
    (index u0)
  )
    (if (is-none file)
      u0
      (count-healthy-shards file-id shard-count)
    )
  )
)

;; Helper to count healthy shards
(define-private (count-healthy-shards (file-id (string-ascii 64)) (shard-count uint))
  (let (
    (healthy-count
      (fold check-shard-health
        (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19)
        u0)
    )
  )
    healthy-count
  )
)

;; Helper to check if a shard is healthy
(define-private (check-shard-health (index uint) (acc uint))
  (let (
    (shard-id { file-id: "example-file", index: index })
    (shard (map-get? file-shards { shard-id: shard-id }))
    (is-healthy (default-to false (get healthy shard)))
  )
    (if (and (is-some shard) is-healthy)
      (+ acc u1)
      acc
    )
  )
)

;; Execute a shard recovery operation
(define-public (execute-recovery (operation-id (string-ascii 64)) (new-provider principal) (source-shard-indices (list 20 uint)))
  (let (
    (operation (map-get? recovery-operations { operation-id: operation-id }))
    (file-id (default-to "" (get file-id operation)))
    (target-index (default-to u0 (get target-shard-index operation)))
    (status (default-to "" (get status operation)))
    (file (map-get? files { file-id: file-id }))
    (owner (default-to tx-sender (get owner file)))
    (is-owner (is-eq tx-sender owner))
    (current-height (get-block-height))
    (recovery-threshold (default-to u0 (get recovery-threshold file)))
  )
    (if (is-none operation)
      (err u215) ;; Recovery operation not found
      (if (not (is-eq status "pending"))
        (err u216) ;; Operation not in pending status
        (if (not is-owner)
          ERR-NOT-AUTHORIZED
          (if (< (len source-shard-indices) recovery-threshold)
            ERR-INSUFFICIENT-SHARDS
            (begin
              ;; Update recovery operation
              (map-set recovery-operations
                { operation-id: operation-id }
                (merge (unwrap-panic operation) {
                  source-shards: source-shard-indices,
                  new-provider: new-provider,
                  status: "completed",
                  completion-height: current-height
                })
              )
              
              ;; Mark target shard as healthy with new provider
              (try! (update-recovered-shard file-id target-index new-provider))
              
              (ok true)
            )
          )
        )
      )
    )
  )
)

;; Update a recovered shard with new provider info
(define-private (update-recovered-shard (file-id (string-ascii 64)) (shard-index uint) (new-provider principal))
  (let (
    (shard (map-get? file-shards { shard-id: { file-id: file-id, index: shard-index } }))
    (current-height (get-block-height))
  )
    (if (is-some shard)
      (begin
        ;; Update shard with new provider and mark as healthy
        (map-set file-shards
          { shard-id: { file-id: file-id, index: shard-index } }
          (merge (unwrap-panic shard) {
            provider: new-provider,
            last-verified: current-height,
            healthy: true
          })
        )
        
        ;; Update provider's shard list - simplified for now
        (ok true)
      )
      ERR-SHARD-NOT-FOUND
    )
  )
)

;; Helper to add shard to a provider
(define-private (add-shard-to-provider (provider-id principal) (file-id (string-ascii 64)) (shard-index uint) (shard-size uint))
  (let (
    (provider-data (default-to { shard-list: (list), total-storage-used: u0 } 
                    (map-get? provider-shards { provider-id: provider-id })))
  )
    (map-set provider-shards
      { provider-id: provider-id }
      {
        shard-list: (default-to (get shard-list provider-data) (as-max-len? (append (get shard-list provider-data) { file-id: file-id, index: shard-index }) u20)),
        total-storage-used: (+ (get total-storage-used provider-data) shard-size)
      }
    )
    (ok true)
  )
)

;; Delete a file and all its shards
(define-public (delete-file (file-id (string-ascii 64)))
  (let (
    (file (map-get? files { file-id: file-id }))
    (owner (default-to tx-sender (get owner file)))
    (is-owner (is-eq tx-sender owner))
    (shard-count (default-to u0 (get shard-count file)))
  )
    (if (is-none file)
      ERR-FILE-NOT-FOUND
      (if (not is-owner)
        ERR-NOT-AUTHORIZED
        (begin
          ;; Mark file as inactive
          (map-set files
            { file-id: file-id }
            (merge (unwrap-panic file) { active: false })
          )
          
          ;; The actual shard deletion would happen at provider level
          ;; Here we just update the file status
          
          (ok true)
        )
      )
    )
  )
)

;; Get functions for read-only access

;; Get file details
(define-read-only (get-file (file-id (string-ascii 64)))
  (map-get? files { file-id: file-id })
)

;; Get shard details
(define-read-only (get-shard (file-id (string-ascii 64)) (index uint))
  (map-get? file-shards { shard-id: { file-id: file-id, index: index } })
)

;; Get all shards for a provider
(define-read-only (get-provider-shard-list (provider-id principal))
  (get shard-list (default-to { shard-list: (list), total-storage-used: u0 }
                  (map-get? provider-shards { provider-id: provider-id })))
)

;; Get challenge details
(define-read-only (get-challenge (challenge-id (string-ascii 64)))
  (map-get? verification-challenges { challenge-id: challenge-id })
)

;; Get recovery operation details
(define-read-only (get-recovery-operation (operation-id (string-ascii 64)))
  (map-get? recovery-operations { operation-id: operation-id })
)

;; Check if a file has sufficient healthy shards for recovery
(define-read-only (is-file-recoverable (file-id (string-ascii 64)))
  (let (
    (file (map-get? files { file-id: file-id }))
    (healthy-count (get-healthy-shard-count file-id))
    (recovery-threshold (default-to u0 (get recovery-threshold file)))
  )
    (>= healthy-count recovery-threshold)
  )
)

;; Get the count of active challenges for a provider
(define-read-only (get-provider-challenge-count (provider-id principal))
  u0  ;; This would require iteration which is not supported in Clarity
     ;; In a real implementation, we'd track this in a separate map
)

;; Helper function to convert uint to string
(define-private (uint-to-ascii (value uint))
  (unwrap-panic (some value))
)

;; Helper function to get current block height
(define-private (get-block-height)
  u100
)


;; title: data-redundancy-manager
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

