;; LocalBallot - Decentralized Community Voting for Grassroots Governance

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-VOTE (err u101))
(define-constant ERR-PROPOSAL-EXPIRED (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u104))
(define-constant ERR-INVALID-PROPOSAL (err u105))
(define-constant ERR-ALREADY-MEMBER (err u106))
(define-constant ERR-INVALID-DELEGATION (err u200))
(define-constant ERR-SELF-DELEGATION (err u201))
(define-constant ERR-DELEGATION-NOT-FOUND (err u202))
(define-constant ERR-INVALID-CATEGORY (err u300))
(define-constant ERR-CATEGORY-EXISTS (err u301))
(define-constant ERR-CATEGORY-NOT-FOUND (err u302))
(define-constant ERR-TIMELOCK-ACTIVE (err u400))
(define-constant ERR-TIMELOCK-NOT-READY (err u401))
(define-constant ERR-EXECUTION-FAILED (err u402))
(define-constant ERR-PROPOSAL-NOT-PASSED (err u403))

;; Reputation constants
(define-constant REPUTATION-MULTIPLIER u10)
(define-constant MAX-REPUTATION-WEIGHT u5)
(define-constant DECAY-PERIOD u1440)
(define-constant PROPOSAL-SUCCESS-BONUS u50)
(define-constant VOTING-PARTICIPATION-BONUS u10)

;; Contract data
(define-data-var admin principal tx-sender)
(define-data-var proposal-count uint u0)
(define-data-var category-count uint u0)
(define-data-var queue-count uint u0)
(define-data-var timelock-delay uint u144)

;; Maps for proposals
(define-map Proposals
    uint
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        creator: principal,
        start-block: uint,
        end-block: uint,
        yes-votes: uint,
        no-votes: uint,
        status: (string-ascii 20),
        min-votes: uint,
    }
)

;; Track who voted on which proposal
(define-map VoteRegistry
    {
        proposal-id: uint,
        voter: principal,
    }
    bool
)

;; Community membership
(define-map CommunityMembers
    principal
    {
        joined-block: uint,
        reputation: uint,
        active: bool,
    }
)

;; Reputation system
(define-map ReputationHistory
    principal
    {
        proposals-created: uint,
        proposals-passed: uint,
        votes-cast: uint,
        last-activity-block: uint,
        total-delegations-received: uint,
    }
)

(define-map ReputationScores
    principal
    uint
)

;; Delegation system
(define-map Delegations
    principal
    {
        delegate: principal,
        delegation-block: uint,
        active: bool,
    }
)

(define-map DelegationPower
    principal
    {
        delegated-votes: uint,
        total-power: uint,
    }
)

;; Category system
(define-map Categories
    uint
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        min-quorum: uint,
        voting-period: uint,
        active: bool,
    }
)

(define-map CategoryNames
    (string-ascii 50)
    uint
)

(define-map ProposalCategories
    uint
    uint
)

(define-map CategoryProposals
    uint
    (list 100 uint)
)

;; Timelock system
(define-map TimeLocks
    uint
    {
        proposal-id: uint,
        execution-block: uint,
        executed: bool,
        cancelled: bool,
    }
)

(define-map ExecutionQueue
    uint
    uint
)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
    (map-get? Proposals proposal-id)
)

(define-read-only (get-member-status (member principal))
    (map-get? CommunityMembers member)
)

(define-read-only (has-voted
        (proposal-id uint)
        (voter principal)
    )
    (is-some (map-get? VoteRegistry {
        proposal-id: proposal-id,
        voter: voter,
    }))
)

(define-read-only (get-voting-power (voter principal))
    (let (
            (base-power (+ u1
                (default-to u0 (get total-power (map-get? DelegationPower voter)))
            ))
            (reputation-weight (get-reputation-weight voter))
        )
        (+ base-power reputation-weight)
    )
)

(define-read-only (get-reputation-score (member principal))
    (default-to u0 (map-get? ReputationScores member))
)

(define-read-only (get-reputation-history (member principal))
    (map-get? ReputationHistory member)
)

(define-read-only (get-reputation-weight (member principal))
    (let (
            (score (get-reputation-score member))
            (calculated-weight (/ score REPUTATION-MULTIPLIER))
        )
        (if (> score u0)
            (if (> calculated-weight MAX-REPUTATION-WEIGHT)
                MAX-REPUTATION-WEIGHT
                calculated-weight
            )
            u0
        )
    )
)

(define-read-only (get-delegation (delegator principal))
    (map-get? Delegations delegator)
)

(define-read-only (get-category (category-id uint))
    (map-get? Categories category-id)
)

(define-read-only (get-category-by-name (name (string-ascii 50)))
    (match (map-get? CategoryNames name)
        category-id (map-get? Categories category-id)
        none
    )
)

(define-read-only (get-proposal-category (proposal-id uint))
    (map-get? ProposalCategories proposal-id)
)

(define-read-only (get-proposals-by-category (category-id uint))
    (map-get? CategoryProposals category-id)
)

(define-read-only (get-timelock (proposal-id uint))
    (map-get? TimeLocks proposal-id)
)

(define-read-only (get-timelock-delay)
    (var-get timelock-delay)
)

(define-read-only (is-execution-ready (proposal-id uint))
    (match (map-get? TimeLocks proposal-id)
        timelock (and
            (>= burn-block-height (get execution-block timelock))
            (not (get executed timelock))
            (not (get cancelled timelock))
        )
        false
    )
)

;; Private functions
(define-private (is-member (user principal))
    (match (map-get? CommunityMembers user)
        member (get active member)
        false
    )
)

(define-private (is-admin (user principal))
    (is-eq user (var-get admin))
)

(define-private (is-category-active (category-id uint))
    (match (map-get? Categories category-id)
        category (get active category)
        false
    )
)

(define-private (update-delegation-power
        (delegate principal)
        (increase bool)
    )
    (let ((current-power (default-to {
            delegated-votes: u0,
            total-power: u0,
        }
            (map-get? DelegationPower delegate)
        )))
        (if increase
            (begin
                (map-set DelegationPower delegate {
                    delegated-votes: (+ (get delegated-votes current-power) u1),
                    total-power: (+ (get total-power current-power) u1),
                })
                (unwrap-panic (update-reputation delegate VOTING-PARTICIPATION-BONUS
                    "delegation-received"
                ))
            )
            (map-set DelegationPower delegate {
                delegated-votes: (if (> (get delegated-votes current-power) u0)
                    (- (get delegated-votes current-power) u1)
                    u0
                ),
                total-power: (if (> (get total-power current-power) u0)
                    (- (get total-power current-power) u1)
                    u0
                ),
            })
        )
        (ok true)
    )
)

(define-private (update-reputation
        (member principal)
        (bonus uint)
        (activity-type (string-ascii 50))
    )
    (let (
            (current-history (default-to {
                proposals-created: u0,
                proposals-passed: u0,
                votes-cast: u0,
                last-activity-block: u0,
                total-delegations-received: u0,
            }
                (map-get? ReputationHistory member)
            ))
            (current-score (get-reputation-score member))
            (decay-factor (calculate-decay-factor member))
            (adjusted-score (* current-score decay-factor))
            (new-score (+ adjusted-score bonus))
        )
        (map-set ReputationScores member new-score)
        (map-set ReputationHistory member
            (merge current-history {
                last-activity-block: burn-block-height,
                proposals-created: (if (is-eq activity-type "proposal-created")
                    (+ (get proposals-created current-history) u1)
                    (get proposals-created current-history)
                ),
                proposals-passed: (if (is-eq activity-type "proposal-passed")
                    (+ (get proposals-passed current-history) u1)
                    (get proposals-passed current-history)
                ),
                votes-cast: (if (is-eq activity-type "vote-cast")
                    (+ (get votes-cast current-history) u1)
                    (get votes-cast current-history)
                ),
                total-delegations-received: (if (is-eq activity-type "delegation-received")
                    (+ (get total-delegations-received current-history) u1)
                    (get total-delegations-received current-history)
                ),
            })
        )
        (ok true)
    )
)

(define-private (calculate-decay-factor (member principal))
    (let ((history (get-reputation-history member)))
        (match history
            member-history (let (
                    (blocks-since-activity (- burn-block-height (get last-activity-block member-history)))
                    (decay-amount (/ blocks-since-activity DECAY-PERIOD))
                    (remaining-factor (if (> decay-amount u50)
                        u50
                        (- u100 decay-amount)
                    ))
                )
                (if (>= blocks-since-activity DECAY-PERIOD)
                    (if (< remaining-factor u50)
                        u50
                        remaining-factor
                    )
                    u100
                )
            )
            u100
        )
    )
)

;; Community management
(define-public (join-community)
    (let ((current-block burn-block-height))
        (asserts! (is-none (get-member-status tx-sender)) ERR-ALREADY-MEMBER)
        (map-set CommunityMembers tx-sender {
            joined-block: current-block,
            reputation: u1,
            active: true,
        })
        (map-set ReputationHistory tx-sender {
            proposals-created: u0,
            proposals-passed: u0,
            votes-cast: u0,
            last-activity-block: current-block,
            total-delegations-received: u0,
        })
        (map-set ReputationScores tx-sender u10)
        (ok true)
    )
)

;; Proposal creation
(define-public (create-proposal
        (title (string-ascii 100))
        (description (string-ascii 500))
        (duration uint)
        (min-votes uint)
    )
    (let (
            (new-id (+ (var-get proposal-count) u1))
            (start-block burn-block-height)
            (end-block (+ start-block duration))
        )
        (asserts! (is-member tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (> (len title) u0) ERR-INVALID-PROPOSAL)
        (asserts! (> duration u0) ERR-INVALID-PROPOSAL)
        (map-set Proposals new-id {
            title: title,
            description: description,
            creator: tx-sender,
            start-block: start-block,
            end-block: end-block,
            yes-votes: u0,
            no-votes: u0,
            status: "active",
            min-votes: min-votes,
        })
        (var-set proposal-count new-id)
        (unwrap-panic (update-reputation tx-sender VOTING-PARTICIPATION-BONUS
            "proposal-created"
        ))
        (ok new-id)
    )
)

(define-public (create-categorized-proposal
        (title (string-ascii 100))
        (description (string-ascii 500))
        (category-id uint)
    )
    (let (
            (category (unwrap! (map-get? Categories category-id) ERR-CATEGORY-NOT-FOUND))
            (new-id (+ (var-get proposal-count) u1))
            (start-block burn-block-height)
            (end-block (+ start-block (get voting-period category)))
            (current-proposals (default-to (list) (map-get? CategoryProposals category-id)))
        )
        (asserts! (is-member tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (> (len title) u0) ERR-INVALID-PROPOSAL)
        (asserts! (get active category) ERR-INVALID-CATEGORY)
        (map-set Proposals new-id {
            title: title,
            description: description,
            creator: tx-sender,
            start-block: start-block,
            end-block: end-block,
            yes-votes: u0,
            no-votes: u0,
            status: "active",
            min-votes: (get min-quorum category),
        })
        (map-set ProposalCategories new-id category-id)
        (map-set CategoryProposals category-id
            (unwrap! (as-max-len? (append current-proposals new-id) u100)
                ERR-INVALID-PROPOSAL
            ))
        (var-set proposal-count new-id)
        (ok new-id)
    )
)

;; Voting
(define-public (cast-vote
        (proposal-id uint)
        (vote bool)
    )
    (let (
            (proposal (unwrap! (map-get? Proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (current-block burn-block-height)
            (voting-power (get-voting-power tx-sender))
        )
        (asserts! (is-member tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (not (has-voted proposal-id tx-sender)) ERR-ALREADY-VOTED)
        (asserts! (<= current-block (get end-block proposal))
            ERR-PROPOSAL-EXPIRED
        )
        (asserts! (is-eq (get status proposal) "active") ERR-INVALID-VOTE)

        (map-set VoteRegistry {
            proposal-id: proposal-id,
            voter: tx-sender,
        }
            true
        )

        (if vote
            (map-set Proposals proposal-id
                (merge proposal { yes-votes: (+ (get yes-votes proposal) voting-power) })
            )
            (map-set Proposals proposal-id
                (merge proposal { no-votes: (+ (get no-votes proposal) voting-power) })
            )
        )
        (unwrap-panic (update-reputation tx-sender VOTING-PARTICIPATION-BONUS "vote-cast"))
        (ok true)
    )
)

(define-public (finalize-proposal (proposal-id uint))
    (let (
            (proposal (unwrap! (map-get? Proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (current-block burn-block-height)
            (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
        )
        (asserts! (>= current-block (get end-block proposal))
            ERR-PROPOSAL-EXPIRED
        )
        (asserts! (>= total-votes (get min-votes proposal)) ERR-INVALID-VOTE)
        (asserts! (is-eq (get status proposal) "active") ERR-INVALID-VOTE)

        (map-set Proposals proposal-id
            (merge proposal { status: (if (> (get yes-votes proposal) (get no-votes proposal))
                "passed"
                "rejected"
            ) }
            ))
        (if (> (get yes-votes proposal) (get no-votes proposal))
            (unwrap-panic (update-reputation (get creator proposal) PROPOSAL-SUCCESS-BONUS
                "proposal-passed"
            ))
            true
        )
        (ok true)
    )
)

;; Delegation
(define-public (delegate-vote (delegate principal))
    (let ((current-block burn-block-height))
        (asserts! (is-member tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-member delegate) ERR-NOT-AUTHORIZED)
        (asserts! (not (is-eq tx-sender delegate)) ERR-SELF-DELEGATION)

        (match (map-get? Delegations tx-sender)
            existing-delegation (begin
                (unwrap-panic (update-delegation-power (get delegate existing-delegation) false))
                (unwrap-panic (update-delegation-power delegate true))
                (map-set Delegations tx-sender {
                    delegate: delegate,
                    delegation-block: current-block,
                    active: true,
                })
            )
            (begin
                (unwrap-panic (update-delegation-power delegate true))
                (map-set Delegations tx-sender {
                    delegate: delegate,
                    delegation-block: current-block,
                    active: true,
                })
            )
        )
        (ok true)
    )
)

(define-public (revoke-delegation)
    (let ((delegation (unwrap! (map-get? Delegations tx-sender) ERR-DELEGATION-NOT-FOUND)))
        (asserts! (get active delegation) ERR-INVALID-DELEGATION)
        (unwrap-panic (update-delegation-power (get delegate delegation) false))
        (map-set Delegations tx-sender (merge delegation { active: false }))
        (ok true)
    )
)

;; Category management
(define-public (create-category
        (name (string-ascii 50))
        (description (string-ascii 200))
        (min-quorum uint)
        (voting-period uint)
    )
    (let ((new-id (+ (var-get category-count) u1)))
        (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (> (len name) u0) ERR-INVALID-CATEGORY)
        (asserts! (is-none (map-get? CategoryNames name)) ERR-CATEGORY-EXISTS)
        (asserts! (> voting-period u0) ERR-INVALID-CATEGORY)

        (map-set Categories new-id {
            name: name,
            description: description,
            min-quorum: min-quorum,
            voting-period: voting-period,
            active: true,
        })
        (map-set CategoryNames name new-id)
        (map-set CategoryProposals new-id (list))
        (var-set category-count new-id)
        (ok new-id)
    )
)

(define-public (toggle-category-status (category-id uint))
    (let ((category (unwrap! (map-get? Categories category-id) ERR-CATEGORY-NOT-FOUND)))
        (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
        (map-set Categories category-id
            (merge category { active: (not (get active category)) })
        )
        (ok true)
    )
)

;; Timelock system
(define-public (queue-execution (proposal-id uint))
    (let (
            (proposal (unwrap! (map-get? Proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (current-block burn-block-height)
            (execution-block (+ current-block (var-get timelock-delay)))
            (queue-id (+ (var-get queue-count) u1))
        )
        (asserts! (is-eq (get status proposal) "passed") ERR-PROPOSAL-NOT-PASSED)
        (asserts! (is-none (map-get? TimeLocks proposal-id)) ERR-TIMELOCK-ACTIVE)

        (map-set TimeLocks proposal-id {
            proposal-id: proposal-id,
            execution-block: execution-block,
            executed: false,
            cancelled: false,
        })
        (map-set ExecutionQueue queue-id proposal-id)
        (var-set queue-count queue-id)
        (ok execution-block)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let (
            (timelock (unwrap! (map-get? TimeLocks proposal-id) ERR-TIMELOCK-NOT-READY))
            (current-block burn-block-height)
        )
        (asserts! (>= current-block (get execution-block timelock))
            ERR-TIMELOCK-NOT-READY
        )
        (asserts! (not (get executed timelock)) ERR-EXECUTION-FAILED)
        (asserts! (not (get cancelled timelock)) ERR-EXECUTION-FAILED)

        (map-set TimeLocks proposal-id (merge timelock { executed: true }))
        (ok true)
    )
)

(define-public (cancel-execution (proposal-id uint))
    (let ((timelock (unwrap! (map-get? TimeLocks proposal-id) ERR-TIMELOCK-NOT-READY)))
        (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (not (get executed timelock)) ERR-EXECUTION-FAILED)
        (map-set TimeLocks proposal-id (merge timelock { cancelled: true }))
        (ok true)
    )
)

(define-public (update-timelock-delay (new-delay uint))
    (begin
        (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (> new-delay u0) ERR-INVALID-PROPOSAL)
        (var-set timelock-delay new-delay)
        (ok true)
    )
)

;; Admin functions
(define-public (update-admin (new-admin principal))
    (begin
        (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
        (var-set admin new-admin)
        (ok true)
    )
)

;; Reputation management
(define-public (update-member-reputation
        (member principal)
        (bonus uint)
    )
    (begin
        (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
        (unwrap-panic (update-reputation member bonus "admin-bonus"))
        (ok true)
    )
)
