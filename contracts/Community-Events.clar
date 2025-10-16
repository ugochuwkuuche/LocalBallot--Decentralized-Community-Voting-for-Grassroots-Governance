;; Community Events System - Independent Feature for LocalBallot
;; Enables community members to create, manage, and track local events

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-EVENT-NOT-FOUND (err u101))
(define-constant ERR-INVALID-EVENT (err u102))
(define-constant ERR-EVENT-EXPIRED (err u103))
(define-constant ERR-ALREADY-REGISTERED (err u104))
(define-constant ERR-NOT-REGISTERED (err u105))
(define-constant ERR-EVENT-FULL (err u106))
(define-constant ERR-REGISTRATION-CLOSED (err u107))
(define-constant ERR-INVALID-CAPACITY (err u108))
(define-constant ERR-INVALID-DATE (err u109))

;; Event type constants
(define-constant EVENT-TYPE-MEETING "meeting")
(define-constant EVENT-TYPE-WORKSHOP "workshop")
(define-constant EVENT-TYPE-DISCUSSION "discussion")
(define-constant EVENT-TYPE-SOCIAL "social")
(define-constant EVENT-TYPE-VOLUNTEER "volunteer")

;; Contract data
(define-data-var event-count uint u0)
(define-data-var admin principal tx-sender)

;; Event data structure
(define-map Events
    uint
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        event-type: (string-ascii 20),
        organizer: principal,
        location: (string-ascii 200),
        start-date: uint,
        end-date: uint,
        max-capacity: uint,
        current-attendees: uint,
        registration-deadline: uint,
        is-active: bool,
        is-public: bool,
        created-block: uint,
    }
)

;; Event registrations
(define-map EventRegistrations
    {
        event-id: uint,
        participant: principal,
    }
    {
        registered-block: uint,
        confirmed: bool,
        attended: bool,
    }
)

;; Participant event list
(define-map ParticipantEvents
    principal
    (list 50 uint)
)

;; Event attendee list
(define-map EventAttendees
    uint
    (list 200 principal)
)

;; Event categories for filtering
(define-map EventsByType
    (string-ascii 20)
    (list 100 uint)
)

;; Organizer statistics
(define-map OrganizerStats
    principal
    {
        events-organized: uint,
        total-attendees: uint,
        successful-events: uint,
        reputation-score: uint,
    }
)

;; Read-only functions
(define-read-only (get-event (event-id uint))
    (map-get? Events event-id)
)

(define-read-only (get-event-count)
    (var-get event-count)
)

(define-read-only (get-registration-status 
        (event-id uint) 
        (participant principal)
    )
    (map-get? EventRegistrations {
        event-id: event-id,
        participant: participant,
    })
)

(define-read-only (is-registered 
        (event-id uint) 
        (participant principal)
    )
    (is-some (map-get? EventRegistrations {
        event-id: event-id,
        participant: participant,
    }))
)

(define-read-only (get-event-attendees (event-id uint))
    (default-to (list) (map-get? EventAttendees event-id))
)

(define-read-only (get-participant-events (participant principal))
    (default-to (list) (map-get? ParticipantEvents participant))
)

(define-read-only (get-events-by-type (event-type (string-ascii 20)))
    (default-to (list) (map-get? EventsByType event-type))
)

(define-read-only (get-organizer-stats (organizer principal))
    (map-get? OrganizerStats organizer)
)

(define-read-only (is-registration-open (event-id uint))
    (match (map-get? Events event-id)
        event (and
            (get is-active event)
            (<= burn-block-height (get registration-deadline event))
            (< (get current-attendees event) (get max-capacity event))
        )
        false
    )
)

(define-read-only (is-event-upcoming (event-id uint))
    (match (map-get? Events event-id)
        event (> (get start-date event) burn-block-height)
        false
    )
)

(define-read-only (is-event-active (event-id uint))
    (match (map-get? Events event-id)
        event (and
            (<= (get start-date event) burn-block-height)
            (>= (get end-date event) burn-block-height)
        )
        false
    )
)

;; Private functions
(define-private (is-admin (user principal))
    (is-eq user (var-get admin))
)

(define-private (is-valid-event-type (event-type (string-ascii 20)))
    (or
        (is-eq event-type EVENT-TYPE-MEETING)
        (or
            (is-eq event-type EVENT-TYPE-WORKSHOP)
            (or
                (is-eq event-type EVENT-TYPE-DISCUSSION)
                (or
                    (is-eq event-type EVENT-TYPE-SOCIAL)
                    (is-eq event-type EVENT-TYPE-VOLUNTEER)
                )
            )
        )
    )
)

(define-private (update-organizer-stats 
        (organizer principal) 
        (stat-type (string-ascii 20))
        (increment uint)
    )
    (let ((current-stats (default-to {
            events-organized: u0,
            total-attendees: u0,
            successful-events: u0,
            reputation-score: u0,
        }
        (map-get? OrganizerStats organizer)
    )))
        (map-set OrganizerStats organizer
            (if (is-eq stat-type "event-created")
                (merge current-stats {
                    events-organized: (+ (get events-organized current-stats) increment),
                    reputation-score: (+ (get reputation-score current-stats) u10),
                })
                (if (is-eq stat-type "attendee-added")
                    (merge current-stats {
                        total-attendees: (+ (get total-attendees current-stats) increment),
                        reputation-score: (+ (get reputation-score current-stats) u5),
                    })
                    (if (is-eq stat-type "event-successful")
                        (merge current-stats {
                            successful-events: (+ (get successful-events current-stats) increment),
                            reputation-score: (+ (get reputation-score current-stats) u25),
                        })
                        current-stats
                    )
                )
            )
        )
        (ok true)
    )
)

(define-private (add-to-type-list 
        (event-type (string-ascii 20)) 
        (event-id uint)
    )
    (let ((current-list (get-events-by-type event-type)))
        (map-set EventsByType event-type
            (unwrap! (as-max-len? (append current-list event-id) u100)
                ERR-INVALID-EVENT
            )
        )
        (ok true)
    )
)

;; Public functions
(define-public (create-event
        (title (string-ascii 100))
        (description (string-ascii 500))
        (event-type (string-ascii 20))
        (location (string-ascii 200))
        (start-date uint)
        (end-date uint)
        (max-capacity uint)
        (registration-deadline uint)
        (is-public bool)
    )
    (let ((new-id (+ (var-get event-count) u1)))
        (asserts! (> (len title) u0) ERR-INVALID-EVENT)
        (asserts! (> (len location) u0) ERR-INVALID-EVENT)
        (asserts! (is-valid-event-type event-type) ERR-INVALID-EVENT)
        (asserts! (> start-date burn-block-height) ERR-INVALID-DATE)
        (asserts! (> end-date start-date) ERR-INVALID-DATE)
        (asserts! (> max-capacity u0) ERR-INVALID-CAPACITY)
        (asserts! (> registration-deadline burn-block-height) ERR-INVALID-DATE)
        (asserts! (< registration-deadline start-date) ERR-INVALID-DATE)

        (map-set Events new-id {
            title: title,
            description: description,
            event-type: event-type,
            organizer: tx-sender,
            location: location,
            start-date: start-date,
            end-date: end-date,
            max-capacity: max-capacity,
            current-attendees: u0,
            registration-deadline: registration-deadline,
            is-active: true,
            is-public: is-public,
            created-block: burn-block-height,
        })
        
        (map-set EventAttendees new-id (list))
        (unwrap-panic (add-to-type-list event-type new-id))
        (unwrap-panic (update-organizer-stats tx-sender "event-created" u1))
        (var-set event-count new-id)
        (ok new-id)
    )
)

(define-public (register-for-event (event-id uint))
    (let ((event (unwrap! (map-get? Events event-id) ERR-EVENT-NOT-FOUND)))
        (asserts! (is-registration-open event-id) ERR-REGISTRATION-CLOSED)
        (asserts! (not (is-registered event-id tx-sender)) ERR-ALREADY-REGISTERED)
        (asserts! (< (get current-attendees event) (get max-capacity event)) ERR-EVENT-FULL)

        (map-set EventRegistrations {
            event-id: event-id,
            participant: tx-sender,
        } {
            registered-block: burn-block-height,
            confirmed: true,
            attended: false,
        })

        (let ((current-attendees (get-event-attendees event-id))
              (participant-events (get-participant-events tx-sender)))
            (map-set EventAttendees event-id
                (unwrap! (as-max-len? (append current-attendees tx-sender) u200)
                    ERR-EVENT-FULL
                )
            )
            (map-set ParticipantEvents tx-sender
                (unwrap! (as-max-len? (append participant-events event-id) u50)
                    ERR-INVALID-EVENT
                )
            )
        )

        (map-set Events event-id
            (merge event { current-attendees: (+ (get current-attendees event) u1) })
        )
        
        (unwrap-panic (update-organizer-stats (get organizer event) "attendee-added" u1))
        (ok true)
    )
)

(define-public (unregister-from-event (event-id uint))
    (let ((event (unwrap! (map-get? Events event-id) ERR-EVENT-NOT-FOUND))
          (registration (unwrap! (get-registration-status event-id tx-sender) ERR-NOT-REGISTERED)))
        
        (asserts! (> (get start-date event) burn-block-height) ERR-EVENT-EXPIRED)
        
        (map-delete EventRegistrations {
            event-id: event-id,
            participant: tx-sender,
        })

        (map-set Events event-id
            (merge event { 
                current-attendees: (if (> (get current-attendees event) u0)
                    (- (get current-attendees event) u1)
                    u0
                )
            })
        )
        (ok true)
    )
)

(define-public (mark-attendance 
        (event-id uint) 
        (participant principal)
    )
    (let ((event (unwrap! (map-get? Events event-id) ERR-EVENT-NOT-FOUND))
          (registration (unwrap! (get-registration-status event-id participant) ERR-NOT-REGISTERED)))
        
        (asserts! (or (is-eq tx-sender (get organizer event)) (is-admin tx-sender)) ERR-NOT-AUTHORIZED)
        (asserts! (is-event-active event-id) ERR-INVALID-EVENT)
        
        (map-set EventRegistrations {
            event-id: event-id,
            participant: participant,
        } (merge registration { attended: true }))
        (ok true)
    )
)

(define-public (finalize-event (event-id uint))
    (let ((event (unwrap! (map-get? Events event-id) ERR-EVENT-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get organizer event)) ERR-NOT-AUTHORIZED)
        (asserts! (>= burn-block-height (get end-date event)) ERR-INVALID-EVENT)
        
        (map-set Events event-id (merge event { is-active: false }))
        
        (if (> (get current-attendees event) u0)
            (unwrap-panic (update-organizer-stats (get organizer event) "event-successful" u1))
            true
        )
        (ok true)
    )
)

(define-public (cancel-event (event-id uint))
    (let ((event (unwrap! (map-get? Events event-id) ERR-EVENT-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get organizer event)) ERR-NOT-AUTHORIZED)
        (asserts! (> (get start-date event) burn-block-height) ERR-EVENT-EXPIRED)
        
        (map-set Events event-id (merge event { is-active: false }))
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

(define-public (force-cancel-event (event-id uint))
    (let ((event (unwrap! (map-get? Events event-id) ERR-EVENT-NOT-FOUND)))
        (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
        (map-set Events event-id (merge event { is-active: false }))
        (ok true)
    )
)