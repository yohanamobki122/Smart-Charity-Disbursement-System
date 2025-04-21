(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-milestone-not-approved (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-milestone-already-completed (err u106))
(define-constant err-charity-not-active (err u107))
(define-constant err-invalid-percentage (err u108))
(define-constant err-invalid-milestone-count (err u109))

(define-data-var contract-owner principal tx-sender)

(define-map charities
  { charity-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    total-funds: uint,
    funds-disbursed: uint,
    milestone-count: uint,
    is-active: bool,
    creator: principal
  }
)

(define-map milestones
  { charity-id: uint, milestone-id: uint }
  {
    description: (string-ascii 200),
    percentage: uint,
    is-completed: bool,
    is-approved: bool,
    approver: (optional principal)
  }
)

(define-map charity-donors
  { charity-id: uint, donor: principal }
  { amount: uint }
)

(define-map approvers
  { approver-id: principal }
  { is-active: bool }
)

(define-data-var charity-counter uint u0)

(define-read-only (get-charity (charity-id uint))
  (map-get? charities { charity-id: charity-id })
)

(define-read-only (get-milestone (charity-id uint) (milestone-id uint))
  (map-get? milestones { charity-id: charity-id, milestone-id: milestone-id })
)

(define-read-only (get-donor-contribution (charity-id uint) (donor principal))
  (default-to u0 (get amount (map-get? charity-donors { charity-id: charity-id, donor: donor })))
)

(define-read-only (is-approver (principal principal))
  (default-to false (get is-active (map-get? approvers { approver-id: principal })))
)

(define-read-only (get-owner)
  (var-get contract-owner)
)

(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
    (ok (var-set contract-owner new-owner))
  )
)

(define-public (add-approver (approver principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
    (asserts! (is-none (map-get? approvers { approver-id: approver })) err-already-exists)
    (map-set approvers { approver-id: approver } { is-active: true })
    (ok true)
  )
)

(define-public (remove-approver (approver principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
    (asserts! (is-some (map-get? approvers { approver-id: approver })) err-not-found)
    (map-delete approvers { approver-id: approver })
    (ok true)
  )
)

(define-public (create-charity (name (string-ascii 100)) (description (string-ascii 500)) (milestone-count uint))
  (let
    (
      (charity-id (+ (var-get charity-counter) u1))
    )
    (asserts! (> milestone-count u0) err-invalid-milestone-count)
    (asserts! (<= milestone-count u10) err-invalid-milestone-count)
    (map-set charities
      { charity-id: charity-id }
      {
        name: name,
        description: description,
        total-funds: u0,
        funds-disbursed: u0,
        milestone-count: milestone-count,
        is-active: true,
        creator: tx-sender
      }
    )
    (var-set charity-counter charity-id)
    (ok charity-id)
  )
)

(define-public (add-milestone (charity-id uint) (milestone-id uint) (description (string-ascii 200)) (percentage uint))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
    )
    (asserts! (is-eq (get creator charity) tx-sender) err-unauthorized)
    (asserts! (get is-active charity) err-charity-not-active)
    (asserts! (<= milestone-id (get milestone-count charity)) err-not-found)
    (asserts! (> percentage u0) err-invalid-percentage)
    (asserts! (<= percentage u100) err-invalid-percentage)
    (asserts! (is-none (map-get? milestones { charity-id: charity-id, milestone-id: milestone-id })) err-already-exists)
    
    (map-set milestones
      { charity-id: charity-id, milestone-id: milestone-id }
      {
        description: description,
        percentage: percentage,
        is-completed: false,
        is-approved: false,
        approver: none
      }
    )
    (ok true)
  )
)

(define-public (donate-to-charity (charity-id uint) (amount uint))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (current-donation (get-donor-contribution charity-id tx-sender))
    )
    (asserts! (get is-active charity) err-charity-not-active)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set charity-donors
      { charity-id: charity-id, donor: tx-sender }
      { amount: (+ current-donation amount) }
    )
    
    (map-set charities
      { charity-id: charity-id }
      (merge charity { total-funds: (+ (get total-funds charity) amount) })
    )
    
    (ok true)
  )
)

(define-public (approve-milestone (charity-id uint) (milestone-id uint))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (milestone (unwrap! (map-get? milestones { charity-id: charity-id, milestone-id: milestone-id }) err-not-found))
    )
    (asserts! (is-approver tx-sender) err-unauthorized)
    (asserts! (get is-active charity) err-charity-not-active)
    (asserts! (not (get is-approved milestone)) err-already-exists)
    
    (map-set milestones
      { charity-id: charity-id, milestone-id: milestone-id }
      (merge milestone { is-approved: true, approver: (some tx-sender) })
    )
    
    (ok true)
  )
)

(define-public (complete-milestone (charity-id uint) (milestone-id uint))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (milestone (unwrap! (map-get? milestones { charity-id: charity-id, milestone-id: milestone-id }) err-not-found))
      (disburse-amount (/ (* (get total-funds charity) (get percentage milestone)) u100))
    )
    (asserts! (is-eq (get creator charity) tx-sender) err-unauthorized)
    (asserts! (get is-active charity) err-charity-not-active)
    (asserts! (get is-approved milestone) err-milestone-not-approved)
    (asserts! (not (get is-completed milestone)) err-milestone-already-completed)
    (asserts! (>= (- (get total-funds charity) (get funds-disbursed charity)) disburse-amount) err-insufficient-funds)
    
    (try! (as-contract (stx-transfer? disburse-amount tx-sender (get creator charity))))
    
    (map-set milestones
      { charity-id: charity-id, milestone-id: milestone-id }
      (merge milestone { is-completed: true })
    )
    
    (map-set charities
      { charity-id: charity-id }
      (merge charity { funds-disbursed: (+ (get funds-disbursed charity) disburse-amount) })
    )
    
    (ok true)
  )
)

(define-public (deactivate-charity (charity-id uint))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
    )
    (asserts! (or (is-eq tx-sender (var-get contract-owner)) (is-eq tx-sender (get creator charity))) err-unauthorized)
    
    (map-set charities
      { charity-id: charity-id }
      (merge charity { is-active: false })
    )
    
    (ok true)
  )
)

