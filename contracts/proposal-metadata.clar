(define-map proposals
  { owner: principal, id: uint }
  { title: (string-utf8 80), uri: (string-utf8 256), start: uint, end: uint, quorum: uint, sealed: bool })

(define-map counters
  { owner: principal }
  { next: uint })

(define-constant ERR_EXISTS u100)
(define-constant ERR_NOT_FOUND u101)
(define-constant ERR_SEALED u103)
(define-constant ERR_WINDOW u104)
(define-constant ERR_QUORUM u105)

(define-public (create (title (string-utf8 80)) (uri (string-utf8 256)) (start uint) (end uint) (quorum uint))
  (if (>= start end)
      (err ERR_WINDOW)
      (if (is-eq quorum u0)
          (err ERR_QUORUM)
          (let
              (
                (n (match (map-get? counters { owner: tx-sender }) c (get next c) u0))
                (existing (map-get? proposals { owner: tx-sender, id: n }))
              )
              (if (is-some existing)
                  (err ERR_EXISTS)
                  (begin
                    (map-set counters { owner: tx-sender } { next: (+ n u1) })
                    (map-set proposals { owner: tx-sender, id: n } { title: title, uri: uri, start: start, end: end, quorum: quorum, sealed: false })
                    (ok n)))))))

(define-public (update (id uint) (title (string-utf8 80)) (uri (string-utf8 256)) (start uint) (end uint) (quorum uint))
  (if (>= start end)
      (err ERR_WINDOW)
      (if (is-eq quorum u0)
          (err ERR_QUORUM)
          (match (map-get? proposals { owner: tx-sender, id: id })
            p
            (if (get sealed p)
                (err ERR_SEALED)
                (begin
                  (map-set proposals { owner: tx-sender, id: id } { title: title, uri: uri, start: start, end: end, quorum: quorum, sealed: (get sealed p) })
                  (ok true)))
            (err ERR_NOT_FOUND)))))

(define-public (seal (id uint))
  (match (map-get? proposals { owner: tx-sender, id: id })
    p
    (if (get sealed p)
        (err ERR_SEALED)
        (begin
          (map-set proposals { owner: tx-sender, id: id } { title: (get title p), uri: (get uri p), start: (get start p), end: (get end p), quorum: (get quorum p), sealed: true })
          (ok true)))
    (err ERR_NOT_FOUND)))

(define-read-only (get (owner principal) (id uint))
  (map-get? proposals { owner: owner, id: id }))
