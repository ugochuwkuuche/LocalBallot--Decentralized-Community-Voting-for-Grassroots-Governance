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
