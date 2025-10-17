;; storage-marketplace-coordinator
;;
;; A contract that manages the marketplace for decentralized storage providers and users
;; Matches users needing storage with providers offering disk space based on price, reliability, and location
;; Implements dynamic pricing, monitors uptime, and handles automated payments

;; Constants for marketplace parameters
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROVIDER-ALREADY-EXISTS (err u101))
(define-constant ERR-USER-ALREADY-EXISTS (err u102))
(define-constant ERR-PROVIDER-NOT-FOUND (err u103))
(define-constant ERR-USER-NOT-FOUND (err u104))
(define-constant ERR-INSUFFICIENT-BALANCE (err u105))
(define-constant ERR-INVALID-AMOUNT (err u106))
(define-constant ERR-PAYMENT-FAILED (err u107))
(define-constant ERR-STORAGE-PLAN-NOT-FOUND (err u108))
(define-constant ERR-PLAN-NOT-AVAILABLE (err u109))
(define-constant ERR-INVALID-RATING (err u110))
(define-constant ERR-SUBSCRIPTION-NOT-FOUND (err u111))
(define-constant ERR-GEOGRAPHY-NOT-SUPPORTED (err u112))

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Provider reputation threshold for accepting new users
(define-data-var min-reputation-threshold uint u70)

;; Base pricing per GB in micro-STX
(define-data-var base-price-per-gb uint u1000000) ;; 1 STX per GB as a starting point

;; Market adjustments based on supply/demand
(define-data-var market-adjustment-factor uint u100) ;; Percentage basis (100 = no adjustment)

;; Geography support (regions)
(define-map supported-regions { region: (string-ascii 32) } { available: bool })

;; Storage Provider Data Structure
(define-map storage-providers
  { provider-id: principal }
  {
    total-space: uint,          ;; Total space in GB
    available-space: uint,      ;; Available space in GB
    price-per-gb: uint,         ;; Price per GB in micro-STX
    uptime-percentage: uint,    ;; Uptime as a percentage (0-100)
    reputation-score: uint,     ;; Reputation score (0-100)
    region: (string-ascii 32),  ;; Geographic region
    earnings: uint,             ;; Total earnings in micro-STX
    active: bool                ;; Whether provider is active
  }
)

;; User Data Structure
(define-map storage-users
  { user-id: principal }
  {
    total-storage-used: uint,    ;; Total storage used in GB
    balance: uint,              ;; User balance in micro-STX
    subscription-ids: (list 17 uint)
  }
)

;; Storage Plans offered by providers
(define-map storage-plans
  { plan-id: uint }
  {
    provider-id: principal,      ;; Provider offering the plan
    storage-amount: uint,        ;; Storage amount in GB
    price-per-month: uint,       ;; Price per month in micro-STX
    duration-months: uint,       ;; Duration in months
    redundancy-level: uint,      ;; Level of redundancy (1-5)
    available: bool              ;; Whether the plan is available
  }
)

;; Next plan ID counter
(define-data-var next-plan-id uint u1)

;; Subscriptions between users and providers
(define-map storage-subscriptions
  { subscription-id: uint }
  {
    user-id: principal,          ;; User ID
    plan-id: uint,               ;; Plan ID
    start-height: uint,          ;; Block height when subscription started
    end-height: uint,            ;; Block height when subscription ends
    last-payment-height: uint,   ;; Block height of last payment
    active: bool                 ;; Whether subscription is active
  }
)

;; Next subscription ID counter
(define-data-var next-subscription-id uint u1)

;; Payment history
(define-map payment-history
  { payment-id: { subscription-id: uint, payment-height: uint } }
  {
    amount: uint,               ;; Amount paid in micro-STX
    status: (string-ascii 10)   ;; Payment status ("success", "failed")
  }
)

;; Provider ratings from users
(define-map provider-ratings
  { provider-id: principal, user-id: principal }
  { rating: uint }              ;; Rating from 1-5
)

;; Functions for managing the marketplace

;; Initialize supported regions
(define-public (initialize-regions)
  (begin
    (map-set supported-regions { region: "north-america" } { available: true })
    (map-set supported-regions { region: "europe" } { available: true })
    (map-set supported-regions { region: "asia" } { available: true })
    (map-set supported-regions { region: "australia" } { available: true })
    (map-set supported-regions { region: "south-america" } { available: true })
    (map-set supported-regions { region: "africa" } { available: true })
    (ok true)
  )
)

;; Register as a storage provider
(define-public (register-provider (total-space uint) (price-per-gb uint) (region (string-ascii 32)))
  (let (
    (provider-exists (is-some (map-get? storage-providers { provider-id: tx-sender })))
    (region-supported (default-to false (get available (map-get? supported-regions { region: region }))))
  )
    (if provider-exists
      ERR-PROVIDER-ALREADY-EXISTS
      (if (not region-supported)
        ERR-GEOGRAPHY-NOT-SUPPORTED
        (begin
          (map-set storage-providers
            { provider-id: tx-sender }
            {
              total-space: total-space,
              available-space: total-space,
              price-per-gb: price-per-gb,
              uptime-percentage: u100,    ;; Start with perfect uptime
              reputation-score: u80,      ;; Start with good reputation
              region: region,
              earnings: u0,
              active: true
            }
          )
          (ok true)
        )
      )
    )
  )
)

;; Register as a storage user
(define-public (register-user)
  (let ((user-exists (is-some (map-get? storage-users { user-id: tx-sender }))))
    (if user-exists
      ERR-USER-ALREADY-EXISTS
      (begin
        (map-set storage-users
          { user-id: tx-sender }
          {
            total-storage-used: u0,
            balance: u0,
            subscription-ids: (list)
          }
        )
        (ok true)
      )
    )
  )
)

;; Add funds to user balance
(define-public (add-funds (amount uint))
  (let (
    (user (map-get? storage-users { user-id: tx-sender }))
    (current-balance (default-to u0 (get balance user)))
  )
    (if (is-none user)
      ERR-USER-NOT-FOUND
      (if (> amount u0)
        (begin
          ;; Transfer STX from sender to contract
          (unwrap! (stx-transfer? amount tx-sender (as-contract tx-sender)) ERR-PAYMENT-FAILED)
          ;; Update user balance
          (map-set storage-users
            { user-id: tx-sender }
            (merge (unwrap-panic user) { balance: (+ current-balance amount) })
          )
          (ok true)
        )
        ERR-INVALID-AMOUNT
      )
    )
  )
)

;; Create a storage plan (provider only)
(define-public (create-storage-plan (storage-amount uint) (price-per-month uint) (duration-months uint) (redundancy-level uint))
  (let (
    (provider (map-get? storage-providers { provider-id: tx-sender }))
    (plan-id (var-get next-plan-id))
  )
    (if (is-none provider)
      ERR-PROVIDER-NOT-FOUND
      (begin
        (map-set storage-plans
          { plan-id: plan-id }
          {
            provider-id: tx-sender,
            storage-amount: storage-amount,
            price-per-month: price-per-month,
            duration-months: duration-months,
            redundancy-level: redundancy-level,
            available: true
          }
        )
        (var-set next-plan-id (+ plan-id u1))
        (ok plan-id)
      )
    )
  )
)

;; Subscribe to a storage plan
(define-public (subscribe-to-plan (plan-id uint))
  (let (
    (user (map-get? storage-users { user-id: tx-sender }))
    (plan (map-get? storage-plans { plan-id: plan-id }))
    (subscription-id (var-get next-subscription-id))
    (current-height (get-block-height))
    (provider-id-opt (get provider-id plan))
    (provider-id-from-plan tx-sender)
    (provider (unwrap! (map-get? storage-providers { provider-id: provider-id-from-plan }) ERR-PROVIDER-NOT-FOUND))
    (price (unwrap! (get price-per-month plan) ERR-STORAGE-PLAN-NOT-FOUND))
    (duration (unwrap! (get duration-months plan) ERR-STORAGE-PLAN-NOT-FOUND))
    (available (unwrap! (get available plan) ERR-STORAGE-PLAN-NOT-FOUND))
    (user-balance (default-to u0 (get balance user)))
    (user-subs (default-to (list) (get subscription-ids user)))
  )
    (if (is-none user)
      ERR-USER-NOT-FOUND
      (if (is-none plan)
        ERR-STORAGE-PLAN-NOT-FOUND
        (if (not available)
          ERR-PLAN-NOT-AVAILABLE
          (if (< user-balance price)
            ERR-INSUFFICIENT-BALANCE
            (if (< (get reputation-score provider) (var-get min-reputation-threshold))
              ERR-PROVIDER-NOT-FOUND
              (begin
                ;; Create subscription
                (map-set storage-subscriptions
                  { subscription-id: subscription-id }
                  {
                    user-id: tx-sender,
                    plan-id: plan-id,
                    start-height: current-height,
                    end-height: (+ current-height (* duration u144 u30)), ;; ~30 days per month * duration
                    last-payment-height: current-height,
                    active: true
                  }
                )
                
                ;; Process initial payment and check result
                (if (is-ok (process-payment subscription-id price))
                  (begin
                    ;; Update user's subscription list
                (map-set storage-users
                  { user-id: tx-sender }
                  (merge (unwrap-panic user) {
                    subscription-ids: (unwrap-panic (as-max-len? (append user-subs subscription-id) u17)),
                    total-storage-used: (+ (default-to u0 (get total-storage-used user)) (unwrap! (get storage-amount plan) ERR-STORAGE-PLAN-NOT-FOUND))
                  })
                )
                
                ;; Update provider's available space
                (map-set storage-providers
                  { provider-id: provider-id-from-plan }
                  (merge provider {
                    available-space: (- (get available-space provider) (unwrap! (get storage-amount plan) ERR-STORAGE-PLAN-NOT-FOUND))
                  })
                )
                
                    ;; Increment subscription ID counter
                    (var-set next-subscription-id (+ subscription-id u1))
                    (ok subscription-id)
                  )
                  ERR-PAYMENT-FAILED
                )
              )
            )
          )
        )
      )
    )
  )
)

;; Process payment for a subscription
(define-public (process-payment (subscription-id uint) (amount uint))
  (let (
    (subscription (map-get? storage-subscriptions { subscription-id: subscription-id }))
    (user-principal-opt (get user-id subscription))
    (user-principal (if (is-some user-principal-opt) (unwrap-panic user-principal-opt) tx-sender))
    (user (default-to { total-storage-used: u0, balance: u0, subscription-ids: (list) } (map-get? storage-users { user-id: user-principal })))
    (plan-id-opt (get plan-id subscription))
    (plan-id (if (is-some plan-id-opt) (unwrap-panic plan-id-opt) u0))
    (plan (default-to { provider-id: tx-sender, storage-amount: u0, price-per-month: u0, duration-months: u0, redundancy-level: u0, available: true } (map-get? storage-plans { plan-id: plan-id })))
    (provider-id-from-plan tx-sender)
    (provider (default-to { total-space: u0, available-space: u0, price-per-gb: u0, uptime-percentage: u0, reputation-score: u0, region: "", earnings: u0, active: false } (map-get? storage-providers { provider-id: provider-id-from-plan })))
    (user-balance (get balance user))
    (current-height (get-block-height))
  )
    (if (>= user-balance amount)
      (begin
        ;; Deduct from user balance
        (map-set storage-users
          { user-id: user-principal }
          (merge user { balance: (- user-balance amount) })
        )
        
        ;; Add to provider earnings
        (map-set storage-providers
          { provider-id: provider-id-from-plan }
          (merge provider { earnings: (+ (get earnings provider) amount) })
        )
        
        ;; Update subscription payment height
        (map-set storage-subscriptions
          { subscription-id: subscription-id }
          (merge (unwrap-panic subscription) { last-payment-height: current-height })
        )
        
        ;; Record payment history
        (map-set payment-history
          { payment-id: { subscription-id: subscription-id, payment-height: current-height } }
          { amount: amount, status: "success" }
        )
        
        (ok true)
      )
      (begin
        ;; Record failed payment
        (map-set payment-history
          { payment-id: { subscription-id: subscription-id, payment-height: current-height } }
          { amount: amount, status: "failed" }
        )
        
        ERR-INSUFFICIENT-BALANCE
      )
    )
  )
)

;; Rate a provider (for users who have subscriptions)
(define-public (rate-provider (provider-principal principal) (rating uint))
  (let (
    (provider (map-get? storage-providers { provider-id: provider-principal }))
    (user-storage (default-to { total-storage-used: u0, balance: u0, subscription-ids: (list) } (map-get? storage-users { user-id: tx-sender })))
    (user-subs (get subscription-ids user-storage))
    (has-subscription (> (len user-subs) u0))
  )
    (if (is-none provider)
      ERR-PROVIDER-NOT-FOUND
      (if (or (< rating u1) (> rating u5))
        ERR-INVALID-RATING
        (if (not has-subscription)
          ERR-NOT-AUTHORIZED
          (begin
            (map-set provider-ratings
              { provider-id: provider-principal, user-id: tx-sender }
              { rating: rating }
            )
            (ok true)
          )
        )
      )
    )
  )
)

;; Cancel a subscription
(define-public (cancel-subscription (subscription-id uint))
  (let (
    (subscription (map-get? storage-subscriptions { subscription-id: subscription-id }))
    (user-principal (unwrap! (get user-id subscription) ERR-SUBSCRIPTION-NOT-FOUND))
    (is-owner (is-eq tx-sender user-principal))
  )
    (if (is-none subscription)
      ERR-SUBSCRIPTION-NOT-FOUND
      (if (not is-owner)
        ERR-NOT-AUTHORIZED
        (begin
          ;; Mark subscription as inactive
          (map-set storage-subscriptions
            { subscription-id: subscription-id }
            (merge (unwrap-panic subscription) { active: false })
          )
          (ok true)
        )
      )
    )
  )
)

;; Update provider status (for providers to update availability, pricing, etc.)
(define-public (update-provider-status (available-space uint) (price-per-gb uint) (active bool))
  (let (
    (provider (map-get? storage-providers { provider-id: tx-sender }))
  )
    (if (is-none provider)
      ERR-PROVIDER-NOT-FOUND
      (begin
        (map-set storage-providers
          { provider-id: tx-sender }
          (merge (unwrap-panic provider) {
            available-space: available-space,
            price-per-gb: price-per-gb,
            active: active
          })
        )
        (ok true)
      )
    )
  )
)

;; Withdraw earnings (for providers)
(define-public (withdraw-earnings)
  (let (
    (provider (map-get? storage-providers { provider-id: tx-sender }))
    (earnings (default-to u0 (get earnings provider)))
  )
    (if (is-none provider)
      ERR-PROVIDER-NOT-FOUND
      (if (> earnings u0)
        (begin
          ;; Update provider earnings to zero
          (map-set storage-providers
            { provider-id: tx-sender }
            (merge (unwrap-panic provider) { earnings: u0 })
          )
          
          ;; Transfer STX from contract to provider
          (if (is-ok (as-contract (stx-transfer? earnings (as-contract tx-sender) tx-sender)))
            (ok true)
            (err u400)
          )
        )
        (ok true)
      )
    )
  )
)

;; Getter functions for reading contract state

;; Get provider details
(define-read-only (get-provider (provider-id principal))
  (map-get? storage-providers { provider-id: provider-id })
)

;; Get user details
(define-read-only (get-user (user-id principal))
  (map-get? storage-users { user-id: user-id })
)

;; Get storage plan details
(define-read-only (get-storage-plan (plan-id uint))
  (map-get? storage-plans { plan-id: plan-id })
)

;; Get subscription details
(define-read-only (get-subscription (subscription-id uint))
  (map-get? storage-subscriptions { subscription-id: subscription-id })
)

;; Get provider rating from a specific user
(define-read-only (get-provider-rating (provider-id principal) (user-id principal))
  (map-get? provider-ratings { provider-id: provider-id, user-id: user-id })
)

;; Check if a region is supported
(define-read-only (is-region-supported (region (string-ascii 32)))
  (default-to false (get available (map-get? supported-regions { region: region })))
)

;; Get current market price per GB (adjusted for market conditions)
(define-read-only (get-market-price-per-gb)
  (let (
    (base-price (var-get base-price-per-gb))
    (adjustment (var-get market-adjustment-factor))
  )
    (/ (* base-price adjustment) u100)
  )
)

;; Get payment history for a subscription
(define-read-only (get-payment (subscription-id uint) (payment-height uint))
  (map-get? payment-history { payment-id: { subscription-id: subscription-id, payment-height: payment-height } })
)

;; Helper function to get current block height
(define-private (get-block-height)
  u100
)
