(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-VOTE (err u101))
(define-constant ERR-PROPOSAL-EXPIRED (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u104))
(define-constant ERR-INVALID-PROPOSAL (err u105))

(define-data-var admin principal tx-sender)
(define-data-var proposal-count uint u0)

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

(define-map VoteRegistry
    {
        proposal-id: uint,
        voter: principal,
    }
    bool
)

(define-map CommunityMembers
    principal
    {
        joined-block: uint,
        reputation: uint,
        active: bool,
    }
)

(define-read-only (get-proposal (proposal-id uint))
    (match (map-get? Proposals proposal-id)
        proposal (ok proposal)
        (err ERR-PROPOSAL-NOT-FOUND)
    )
)

(define-read-only (get-member-status (member principal))
    (map-get? CommunityMembers member)
)

(define-read-only (get-vote-status
        (proposal-id uint)
        (voter principal)
    )
    (map-get? VoteRegistry {
        proposal-id: proposal-id,
        voter: voter,
    })
)

(define-private (is-member (user principal))
    (match (map-get? CommunityMembers user)
        member (get active member)
        false
    )
)

(define-private (has-voted
        (proposal-id uint)
        (voter principal)
    )
    (match (map-get? VoteRegistry {
        proposal-id: proposal-id,
        voter: voter,
    })
        vote-status
        vote-status
        false
    )
)

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
        (ok new-id)
    )
)

(define-public (cast-vote
        (proposal-id uint)
        (vote bool)
    )
    (let (
            (proposal (unwrap! (map-get? Proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (current-block burn-block-height)
        )
        (asserts! (is-member tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (not (has-voted proposal-id tx-sender)) ERR-ALREADY-VOTED)
        (asserts! (<= current-block (get end-block proposal))
            ERR-PROPOSAL-EXPIRED
        )
        (map-set VoteRegistry {
            proposal-id: proposal-id,
            voter: tx-sender,
        }
            true
        )
        (if vote
            (map-set Proposals proposal-id
                (merge proposal { yes-votes: (+ (get yes-votes proposal) u1) })
            )
            (map-set Proposals proposal-id
                (merge proposal { no-votes: (+ (get no-votes proposal) u1) })
            )
        )
        (ok true)
    )
)

(define-public (join-community)
    (let ((current-block burn-block-height))
        (asserts! (is-none (get-member-status tx-sender)) ERR-ALREADY-VOTED)
        (map-set CommunityMembers tx-sender {
            joined-block: current-block,
            reputation: u1,
            active: true,
        })
        (ok true)
    )
)

(define-public (finalize-proposal (proposal-id uint))
    (let (
            (proposal (unwrap! (map-get? Proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (current-block burn-block-height)
            (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
        )
        (asserts! (>= current-block (get end-block proposal)) ERR-INVALID-VOTE)
        (asserts! (>= total-votes (get min-votes proposal)) ERR-INVALID-VOTE)
        (map-set Proposals proposal-id
            (merge proposal { status: (if (> (get yes-votes proposal) (get no-votes proposal))
                "passed"
                "rejected"
            ) }
            ))
        (ok true)
    )
)

(define-constant ERR-INVALID-DELEGATION (err u200))
(define-constant ERR-SELF-DELEGATION (err u201))
(define-constant ERR-DELEGATION-NOT-FOUND (err u202))

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
            (map-set DelegationPower delegate {
                delegated-votes: (+ (get delegated-votes current-power) u1),
                total-power: (+ (get total-power current-power) u1),
            })
            (map-set DelegationPower delegate {
                delegated-votes: (- (get delegated-votes current-power) u1),
                total-power: (- (get total-power current-power) u1),
            })
        )
    )
)

(define-public (delegate-vote (delegate principal))
    (let ((current-block burn-block-height))
        (asserts! (is-member tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-member delegate) ERR-NOT-AUTHORIZED)
        (asserts! (not (is-eq tx-sender delegate)) ERR-SELF-DELEGATION)
        (match (map-get? Delegations tx-sender)
            existing-delegation (begin
                (update-delegation-power (get delegate existing-delegation) false)
                (update-delegation-power delegate true)
                (map-set Delegations tx-sender {
                    delegate: delegate,
                    delegation-block: current-block,
                    active: true,
                })
            )
            (begin
                (update-delegation-power delegate true)
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
        (update-delegation-power (get delegate delegation) false)
        (map-set Delegations tx-sender (merge delegation { active: false }))
        (ok true)
    )
)

(define-public (cast-delegated-vote
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
        (ok true)
    )
)

(define-read-only (get-delegation (delegator principal))
    (map-get? Delegations delegator)
)

(define-read-only (get-voting-power (voter principal))
    (+ u1 (default-to u0 (get total-power (map-get? DelegationPower voter))))
)

(define-constant ERR-INVALID-CATEGORY (err u300))
(define-constant ERR-CATEGORY-EXISTS (err u301))
(define-constant ERR-CATEGORY-NOT-FOUND (err u302))

(define-data-var category-count uint u0)

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

(define-public (create-category
        (name (string-ascii 50))
        (description (string-ascii 200))
        (min-quorum uint)
        (voting-period uint)
    )
    (let ((new-id (+ (var-get category-count) u1)))
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (asserts! (> (len name) u0) ERR-INVALID-CATEGORY)
        (asserts! (is-none (map-get? CategoryNames name)) ERR-CATEGORY-EXISTS)
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

(define-read-only (get-active-categories)
    (var-get category-count)
)

(define-private (filter-active-categories (category-ids (list 100 uint)))
    (filter is-category-active category-ids)
)

(define-private (is-category-active (category-id uint))
    (match (map-get? Categories category-id)
        category (get active category)
        false
    )
)

(define-public (toggle-category-status (category-id uint))
    (let ((category (unwrap! (map-get? Categories category-id) ERR-CATEGORY-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (map-set Categories category-id
            (merge category { active: (not (get active category)) })
        )
        (ok true)
    )
)

(define-constant ERR-TIMELOCK-ACTIVE (err u400))
(define-constant ERR-TIMELOCK-NOT-READY (err u401))
(define-constant ERR-EXECUTION-FAILED (err u402))
(define-constant ERR-PROPOSAL-NOT-PASSED (err u403))

(define-data-var timelock-delay uint u144)

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

(define-data-var queue-count uint u0)

(define-private (get-proposal-status (proposal-id uint))
    (match (map-get? Proposals proposal-id)
        proposal (get status proposal)
        "not-found"
    )
)

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
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get executed timelock)) ERR-EXECUTION-FAILED)
        (map-set TimeLocks proposal-id (merge timelock { cancelled: true }))
        (ok true)
    )
)

(define-public (update-timelock-delay (new-delay uint))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (var-set timelock-delay new-delay)
        (ok true)
    )
)

(define-read-only (get-timelock (proposal-id uint))
    (map-get? TimeLocks proposal-id)
)

(define-read-only (get-timelock-delay)
    (var-get timelock-delay)
)

(define-read-only (get-execution-queue-size)
    (var-get queue-count)
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
