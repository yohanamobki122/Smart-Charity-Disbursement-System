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
      (is-new-donor (is-eq current-donation u0))
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
    
    (unwrap-panic (update-charity-analytics charity-id is-new-donor amount))
    
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
    
    (unwrap-panic (increment-milestone-completion charity-id))
    
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

(define-constant err-no-funds-to-recover (err u110))
(define-constant err-charity-still-active (err u111))

(define-public (recover-inactive-charity-funds (charity-id uint))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (recoverable-amount (- (get total-funds charity) (get funds-disbursed charity)))
    )
    (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
    (asserts! (not (get is-active charity)) err-charity-still-active)
    (asserts! (> recoverable-amount u0) err-no-funds-to-recover)
    
    (try! (as-contract (stx-transfer? recoverable-amount tx-sender (var-get contract-owner))))
    
    (map-set charities
      { charity-id: charity-id }
      (merge charity { funds-disbursed: (get total-funds charity) })
    )
    
    (ok recoverable-amount)
  )
)



(define-constant err-no-changes (err u112))

(define-public (update-charity-metadata 
    (charity-id uint) 
    (new-name (optional (string-ascii 100))) 
    (new-description (optional (string-ascii 500))))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (final-name (default-to (get name charity) new-name))
      (final-description (default-to (get description charity) new-description))
    )
    (asserts! (is-eq (get creator charity) tx-sender) err-unauthorized)
    (asserts! (get is-active charity) err-charity-not-active)
    (asserts! (or (is-some new-name) (is-some new-description)) err-no-changes)
    
    (map-set charities
      { charity-id: charity-id }
      (merge charity 
        { 
          name: final-name,
          description: final-description
        }
      )
    )
    
    (ok true)
  )
)


(define-constant err-not-verified (err u113))
(define-constant err-verifier-not-authorized (err u114))
(define-constant err-already-verified (err u115))

(define-map charity-verifications
  { charity-id: uint }
  {
    is-verified: bool,
    verified-by: principal,
    verification-date: uint,
    verification-notes: (string-ascii 200)
  }
)

(define-map authorized-verifiers
  { verifier: principal }
  { is-authorized: bool }
)

(define-read-only (is-charity-verified (charity-id uint))
  (default-to false 
    (get is-verified 
      (map-get? charity-verifications { charity-id: charity-id }))))

(define-read-only (get-charity-verification (charity-id uint))
  (map-get? charity-verifications { charity-id: charity-id }))

(define-read-only (is-authorized-verifier (verifier principal))
  (default-to false 
    (get is-authorized 
      (map-get? authorized-verifiers { verifier: verifier }))))

(define-public (add-authorized-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
    (map-set authorized-verifiers 
      { verifier: verifier } 
      { is-authorized: true })
    (ok true)))

(define-public (remove-authorized-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) err-owner-only)
    (asserts! (is-some (map-get? authorized-verifiers { verifier: verifier })) err-not-found)
    (map-delete authorized-verifiers { verifier: verifier })
    (ok true)))

(define-public (verify-charity 
    (charity-id uint) 
    (verification-notes (string-ascii 200)))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (existing-verification (map-get? charity-verifications { charity-id: charity-id }))
    )
    (asserts! (is-authorized-verifier tx-sender) err-verifier-not-authorized)
    (asserts! (get is-active charity) err-charity-not-active)
    (asserts! (is-none existing-verification) err-already-verified)
    
    (map-set charity-verifications
      { charity-id: charity-id }
      {
        is-verified: true,
        verified-by: tx-sender,
        verification-date: stacks-block-height,
        verification-notes: verification-notes
      })
    (ok true)))

(define-public (revoke-charity-verification (charity-id uint))
  (let
    (
      (verification (unwrap! (map-get? charity-verifications { charity-id: charity-id }) err-not-found))
    )
    (asserts! (or 
      (is-eq tx-sender (var-get contract-owner))
      (is-eq tx-sender (get verified-by verification))) err-unauthorized)
    
    (map-delete charity-verifications { charity-id: charity-id })
    (ok true)))

(define-public (donate-to-verified-charity-only (charity-id uint) (amount uint))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (current-donation (get-donor-contribution charity-id tx-sender))
    )
    (asserts! (get is-active charity) err-charity-not-active)
    (asserts! (is-charity-verified charity-id) err-not-verified)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set charity-donors
      { charity-id: charity-id, donor: tx-sender }
      { amount: (+ current-donation amount) })
    
    (map-set charities
      { charity-id: charity-id }
      (merge charity { total-funds: (+ (get total-funds charity) amount) }))
    
    (unwrap-panic (update-charity-analytics charity-id (is-eq current-donation u0) amount))
    
    (ok true)))

(define-map charity-analytics
  { charity-id: uint }
  {
    total-donors: uint,
    avg-donation-amount: uint,
    days-since-creation: uint,
    milestones-completed: uint,
    funding-efficiency-score: uint,
    last-activity-block: uint
  }
)

(define-read-only (get-charity-analytics (charity-id uint))
  (map-get? charity-analytics { charity-id: charity-id })
)

(define-read-only (calculate-funding-velocity (charity-id uint))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (analytics (map-get? charity-analytics { charity-id: charity-id }))
    )
    (match analytics
      some-analytics
        (if (> (get days-since-creation some-analytics) u0)
          (ok (/ (get total-funds charity) (get days-since-creation some-analytics)))
          (ok u0))
      (ok u0)
    )
  )
)

(define-read-only (get-milestone-completion-rate (charity-id uint))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (analytics (map-get? charity-analytics { charity-id: charity-id }))
    )
    (match analytics
      some-analytics
        (if (> (get milestone-count charity) u0)
          (ok (/ (* (get milestones-completed some-analytics) u100) (get milestone-count charity)))
          (ok u0))
      (ok u0)
    )
  )
)

(define-read-only (get-donor-engagement-score (charity-id uint))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (analytics (map-get? charity-analytics { charity-id: charity-id }))
    )
    (match analytics
      some-analytics
        (if (> (get total-donors some-analytics) u0)
          (ok (/ (get total-funds charity) (get total-donors some-analytics)))
          (ok u0))
      (ok u0)
    )
  )
)

(define-private (update-charity-analytics (charity-id uint) (is-new-donor bool) (donation-amount uint))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (current-analytics (default-to 
        {
          total-donors: u0,
          avg-donation-amount: u0,
          days-since-creation: u1,
          milestones-completed: u0,
          funding-efficiency-score: u0,
          last-activity-block: stacks-block-height
        }
        (map-get? charity-analytics { charity-id: charity-id })))
      (new-donor-count (if is-new-donor (+ (get total-donors current-analytics) u1) (get total-donors current-analytics)))
      (new-avg-donation (if (> new-donor-count u0) 
        (/ (get total-funds charity) new-donor-count) 
        u0))
      (efficiency-score (if (> (get total-funds charity) u0)
        (/ (* (get funds-disbursed charity) u100) (get total-funds charity))
        u0))
    )
    (map-set charity-analytics
      { charity-id: charity-id }
      {
        total-donors: new-donor-count,
        avg-donation-amount: new-avg-donation,
        days-since-creation: (get days-since-creation current-analytics),
        milestones-completed: (get milestones-completed current-analytics),
        funding-efficiency-score: efficiency-score,
        last-activity-block: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-private (increment-milestone-completion (charity-id uint))
  (let
    (
      (current-analytics (default-to 
        {
          total-donors: u0,
          avg-donation-amount: u0,
          days-since-creation: u1,
          milestones-completed: u0,
          funding-efficiency-score: u0,
          last-activity-block: stacks-block-height
        }
        (map-get? charity-analytics { charity-id: charity-id })))
    )
    (map-set charity-analytics
      { charity-id: charity-id }
      (merge current-analytics 
        { 
          milestones-completed: (+ (get milestones-completed current-analytics) u1),
          last-activity-block: stacks-block-height
        }
      )
    )
    (ok true)
  )
)

(define-public (initialize-charity-analytics (charity-id uint))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
    )
    (asserts! (is-eq (get creator charity) tx-sender) err-unauthorized)
    (asserts! (is-none (map-get? charity-analytics { charity-id: charity-id })) err-already-exists)
    
    (map-set charity-analytics
      { charity-id: charity-id }
      {
        total-donors: u0,
        avg-donation-amount: u0,
        days-since-creation: u1,
        milestones-completed: u0,
        funding-efficiency-score: u0,
        last-activity-block: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Impact Reporting System Constants
(define-constant err-report-not-found (err u116))
(define-constant err-invalid-report-type (err u117))
(define-constant err-report-already-exists (err u118))
(define-constant err-invalid-beneficiary-count (err u119))
(define-constant err-report-not-verified (err u120))

;; Impact report types: 1=Educational, 2=Healthcare, 3=Environmental, 4=Social, 5=Economic
(define-map impact-reports
  { charity-id: uint, report-id: uint }
  {
    report-type: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    beneficiaries-reached: uint,
    funds-utilized: uint,
    measurable-outcome: (string-ascii 200),
    outcome-value: uint,
    reporting-period-start: uint,
    reporting-period-end: uint,
    submission-date: uint,
    is-verified: bool,
    verified-by: (optional principal)
  }
)

;; Counter for impact reports per charity
(define-map charity-report-counters
  { charity-id: uint }
  { next-report-id: uint }
)

;; Aggregated impact data per charity
(define-map charity-impact-summary
  { charity-id: uint }
  {
    total-reports: uint,
    total-beneficiaries: uint,
    total-funds-reported: uint,
    verified-reports: uint,
    last-report-date: uint,
    impact-score: uint
  }
)

;; Read-only functions for impact reporting
(define-read-only (get-impact-report (charity-id uint) (report-id uint))
  (map-get? impact-reports { charity-id: charity-id, report-id: report-id })
)

(define-read-only (get-charity-impact-summary (charity-id uint))
  (map-get? charity-impact-summary { charity-id: charity-id })
)

(define-read-only (get-charity-report-count (charity-id uint))
  (default-to u0 
    (get next-report-id 
      (map-get? charity-report-counters { charity-id: charity-id })))
)

(define-read-only (calculate-charity-impact-score (charity-id uint))
  (let
    (
      (summary (map-get? charity-impact-summary { charity-id: charity-id }))
    )
    (match summary
      some-summary
        (let
          (
            (verified-percentage (if (> (get total-reports some-summary) u0)
              (/ (* (get verified-reports some-summary) u100) (get total-reports some-summary))
              u0))
            (beneficiary-factor (if (> (get total-beneficiaries some-summary) u1000)
              u100
              (/ (get total-beneficiaries some-summary) u10)))
            (reporting-frequency (if (> (get total-reports some-summary) u0)
              (if (< (* (get total-reports some-summary) u10) u50)
                (* (get total-reports some-summary) u10)
                u50)
              u0))
          )
          (ok (+ verified-percentage beneficiary-factor reporting-frequency))
        )
      (ok u0)
    )
  )
)

;; Submit new impact report
(define-public (submit-impact-report 
    (charity-id uint)
    (report-type uint)
    (title (string-ascii 100))
    (description (string-ascii 500))
    (beneficiaries-reached uint)
    (funds-utilized uint)
    (measurable-outcome (string-ascii 200))
    (outcome-value uint)
    (reporting-period-start uint)
    (reporting-period-end uint))
  (let
    (
      (charity (unwrap! (map-get? charities { charity-id: charity-id }) err-not-found))
      (current-counter (default-to { next-report-id: u1 } 
        (map-get? charity-report-counters { charity-id: charity-id })))
      (report-id (get next-report-id current-counter))
    )
    ;; Validate inputs
    (asserts! (is-eq (get creator charity) tx-sender) err-unauthorized)
    (asserts! (and (>= report-type u1) (<= report-type u5)) err-invalid-report-type)
    (asserts! (<= beneficiaries-reached u1000000) err-invalid-beneficiary-count)
    (asserts! (<= reporting-period-start reporting-period-end) err-invalid-percentage)
    (asserts! (is-none (map-get? impact-reports { charity-id: charity-id, report-id: report-id })) err-report-already-exists)
    
    ;; Create impact report
    (map-set impact-reports
      { charity-id: charity-id, report-id: report-id }
      {
        report-type: report-type,
        title: title,
        description: description,
        beneficiaries-reached: beneficiaries-reached,
        funds-utilized: funds-utilized,
        measurable-outcome: measurable-outcome,
        outcome-value: outcome-value,
        reporting-period-start: reporting-period-start,
        reporting-period-end: reporting-period-end,
        submission-date: stacks-block-height,
        is-verified: false,
        verified-by: none
      }
    )
    
    ;; Update report counter
    (map-set charity-report-counters
      { charity-id: charity-id }
      { next-report-id: (+ report-id u1) }
    )
    
    ;; Update impact summary
    (unwrap-panic (update-charity-impact-summary charity-id beneficiaries-reached funds-utilized false))
    
    (ok report-id)
  )
)

;; Verify impact report (only by authorized verifiers)
(define-public (verify-impact-report (charity-id uint) (report-id uint))
  (let
    (
      (report (unwrap! (map-get? impact-reports { charity-id: charity-id, report-id: report-id }) err-report-not-found))
    )
    (asserts! (is-authorized-verifier tx-sender) err-verifier-not-authorized)
    (asserts! (not (get is-verified report)) err-already-verified)
    
    ;; Mark report as verified
    (map-set impact-reports
      { charity-id: charity-id, report-id: report-id }
      (merge report { is-verified: true, verified-by: (some tx-sender) })
    )
    
    ;; Update impact summary for verification
    (unwrap-panic (update-charity-impact-summary charity-id u0 u0 true))
    
    (ok true)
  )
)

;; Revoke report verification
(define-public (revoke-impact-report-verification (charity-id uint) (report-id uint))
  (let
    (
      (report (unwrap! (map-get? impact-reports { charity-id: charity-id, report-id: report-id }) err-report-not-found))
    )
    (asserts! (or 
      (is-eq tx-sender (var-get contract-owner))
      (is-eq (some tx-sender) (get verified-by report))) err-unauthorized)
    (asserts! (get is-verified report) err-report-not-verified)
    
    ;; Remove verification
    (map-set impact-reports
      { charity-id: charity-id, report-id: report-id }
      (merge report { is-verified: false, verified-by: none })
    )
    
    ;; Update impact summary (decrease verified count)
    (let
      (
        (current-summary (default-to
          { total-reports: u0, total-beneficiaries: u0, total-funds-reported: u0, 
            verified-reports: u0, last-report-date: u0, impact-score: u0 }
          (map-get? charity-impact-summary { charity-id: charity-id })))
      )
      (map-set charity-impact-summary
        { charity-id: charity-id }
        (merge current-summary 
          { verified-reports: (if (> (get verified-reports current-summary) u0)
            (- (get verified-reports current-summary) u1)
            u0) })
      )
    )
    
    (ok true)
  )
)

;; Update charity impact summary (private helper function)
(define-private (update-charity-impact-summary 
    (charity-id uint) 
    (new-beneficiaries uint) 
    (new-funds uint) 
    (is-verification bool))
  (let
    (
      (current-summary (default-to
        { total-reports: u0, total-beneficiaries: u0, total-funds-reported: u0, 
          verified-reports: u0, last-report-date: u0, impact-score: u0 }
        (map-get? charity-impact-summary { charity-id: charity-id })))
      (new-total-reports (if is-verification 
        (get total-reports current-summary)
        (+ (get total-reports current-summary) u1)))
      (new-verified-reports (if is-verification
        (+ (get verified-reports current-summary) u1)
        (get verified-reports current-summary)))
      (impact-score (if (> new-total-reports u0)
        (+ (/ (* new-verified-reports u50) new-total-reports)
           (if (< (/ (+ (get total-beneficiaries current-summary) new-beneficiaries) u100) u30)
             (/ (+ (get total-beneficiaries current-summary) new-beneficiaries) u100)
             u30)
           (if (< (/ new-total-reports u2) u20)
             (/ new-total-reports u2)
             u20))
        u0))
    )
    (map-set charity-impact-summary
      { charity-id: charity-id }
      {
        total-reports: new-total-reports,
        total-beneficiaries: (+ (get total-beneficiaries current-summary) new-beneficiaries),
        total-funds-reported: (+ (get total-funds-reported current-summary) new-funds),
        verified-reports: new-verified-reports,
        last-report-date: stacks-block-height,
        impact-score: impact-score
      }
    )
    (ok true)
  )
)

;; Check if a specific report matches the given type
(define-read-only (is-report-of-type (charity-id uint) (report-id uint) (target-type uint))
  (let
    (
      (report (map-get? impact-reports { charity-id: charity-id, report-id: report-id }))
    )
    (match report
      some-report (is-eq (get report-type some-report) target-type)
      false
    )
  )
)

;; Get total beneficiaries across all verified reports for a charity
(define-read-only (get-verified-beneficiaries-total (charity-id uint))
  (let
    (
      (summary (map-get? charity-impact-summary { charity-id: charity-id }))
    )
    (match summary
      some-summary 
        (if (> (get verified-reports some-summary) u0)
          (get total-beneficiaries some-summary)
          u0)
      u0
    )
  )
)


