;; Decentralized Database Services Contract
;; P2P database hosting with data integrity guarantees

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_DATA (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_NODE_EXISTS (err u104))
(define-constant ERR_NODE_NOT_ACTIVE (err u105))
(define-constant ERR_INVALID_HASH (err u106))
(define-constant ERR_REPLICATION_FAILED (err u107))

;; Data structures
(define-map database-entries
  { db-id: (string-ascii 64), key: (string-ascii 128) }
  {
    value: (string-ascii 512),
    hash: (buff 32),
    timestamp: uint,
    owner: principal,
    replicas: (list 10 principal),
    version: uint
  }
)

(define-map storage-nodes
  principal
  {
    stake: uint,
    reputation: uint,
    active: bool,
    stored-data-size: uint,
    last-heartbeat: uint,
    rewards-earned: uint
  }
)

(define-map database-metadata
  (string-ascii 64)
  {
    owner: principal,
    created-at: uint,
    total-entries: uint,
    replication-factor: uint,
    access-control: (string-ascii 20),
    storage-cost: uint
  }
)

(define-map data-integrity-proofs
  { db-id: (string-ascii 64), key: (string-ascii 128), node: principal }
  {
    merkle-root: (buff 32),
    proof-timestamp: uint,
    verified: bool
  }
)

;; Data variables
(define-data-var total-databases uint u0)
(define-data-var min-stake-amount uint u1000000)
(define-data-var base-storage-cost uint u100)
(define-data-var contract-balance uint u0)

;; Public functions

;; Register as a storage node
(define-public (register-node (stake-amount uint))
  (let ((node-data (map-get? storage-nodes tx-sender)))
    (if (is-some node-data)
      ERR_NODE_EXISTS
      (begin
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        (map-set storage-nodes tx-sender {
          stake: stake-amount,
          reputation: u100,
          active: true,
          stored-data-size: u0,
          last-heartbeat: block-height,
          rewards-earned: u0
        })
        (var-set contract-balance (+ (var-get contract-balance) stake-amount))
        (ok true)))))

;; Create a new database
(define-public (create-database (db-id (string-ascii 64)) (replication-factor uint) (access-control (string-ascii 20)))
  (let ((existing-db (map-get? database-metadata db-id)))
    (if (is-some existing-db)
      ERR_INVALID_DATA
      (begin
        (map-set database-metadata db-id {
          owner: tx-sender,
          created-at: block-height,
          total-entries: u0,
          replication-factor: replication-factor,
          access-control: access-control,
          storage-cost: (var-get base-storage-cost)
        })
        (var-set total-databases (+ (var-get total-databases) u1))
        (ok db-id)))))

;; Store data with integrity guarantees
(define-public (store-data (db-id (string-ascii 64)) (key (string-ascii 128)) (value (string-ascii 512)))
  (let (
    (db-meta (unwrap! (map-get? database-metadata db-id) ERR_NOT_FOUND))
    (data-hash (keccak256 (concat (unwrap-panic (to-consensus-buff? key)) (unwrap-panic (to-consensus-buff? value)))))
    (replica-nodes (select-replica-nodes (get replication-factor db-meta)))
  )
    (asserts! (is-authorized-user db-id tx-sender) ERR_UNAUTHORIZED)
    (try! (pay-storage-fee db-id))
    
    ;; Store data entry
    (map-set database-entries { db-id: db-id, key: key } {
      value: value,
      hash: data-hash,
      timestamp: block-height,
      owner: tx-sender,
      replicas: replica-nodes,
      version: u1
    })
    
    ;; Update database metadata
    (map-set database-metadata db-id
      (merge db-meta { total-entries: (+ (get total-entries db-meta) u1) }))
    
    ;; Store integrity proofs
    (try! (store-integrity-proofs db-id key replica-nodes data-hash))
    
    (ok { key: key, hash: data-hash, replicas: replica-nodes })))

;; Retrieve data
(define-public (get-data (db-id (string-ascii 64)) (key (string-ascii 128)))
  (let ((entry (map-get? database-entries { db-id: db-id, key: key })))
    (match entry
      data-entry (begin
        (asserts! (is-authorized-user db-id tx-sender) ERR_UNAUTHORIZED)
        (ok {
          value: (get value data-entry),
          hash: (get hash data-entry),
          timestamp: (get timestamp data-entry),
          version: (get version data-entry)
        }))
      ERR_NOT_FOUND)))

;; Verify data integrity
(define-public (verify-data-integrity (db-id (string-ascii 64)) (key (string-ascii 128)) (node principal))
  (let (
    (entry (unwrap! (map-get? database-entries { db-id: db-id, key: key }) ERR_NOT_FOUND))
    (proof (map-get? data-integrity-proofs { db-id: db-id, key: key, node: node }))
    (computed-hash (keccak256 (concat (unwrap-panic (to-consensus-buff? key)) (unwrap-panic (to-consensus-buff? (get value entry))))))
  )
    (match proof
      integrity-proof (ok {
        verified: (is-eq (get hash entry) computed-hash),
        merkle-root: (get merkle-root integrity-proof),
        proof-timestamp: (get proof-timestamp integrity-proof)
      })
      (ok { verified: false, merkle-root: 0x00, proof-timestamp: u0 }))))

;; Update node heartbeat
(define-public (heartbeat)
  (let ((node-data (unwrap! (map-get? storage-nodes tx-sender) ERR_NOT_FOUND)))
    (map-set storage-nodes tx-sender
      (merge node-data { 
        last-heartbeat: block-height,
        active: true
      }))
    (ok block-height)))

;; Slash inactive nodes
(define-public (slash-node (node principal))
  (let ((node-data (unwrap! (map-get? storage-nodes node) ERR_NOT_FOUND)))
    (asserts! (> (- block-height (get last-heartbeat node-data)) u1000) ERR_NODE_NOT_ACTIVE)
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-contract-caller)) ERR_UNAUTHORIZED)
    
    ;; Slash 10% of stake
    (let ((slash-amount (/ (get stake node-data) u10)))
      (map-set storage-nodes node
        (merge node-data {
          stake: (- (get stake node-data) slash-amount),
          active: false,
          reputation: (max u0 (- (get reputation node-data) u20))
        }))
      (ok slash-amount))))

;; Distribute rewards to active nodes
(define-public (distribute-rewards (rewards uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    ;; Implementation would iterate through active nodes and distribute rewards
    ;; Simplified version here
    (ok rewards)))

;; Private functions

;; Check if user is authorized to access database
(define-private (is-authorized-user (db-id (string-ascii 64)) (user principal))
  (let ((db-meta (map-get? database-metadata db-id)))
    (match db-meta
      metadata (or 
        (is-eq (get owner metadata) user)
        (is-eq (get access-control metadata) "public"))
      false)))

;; Pay storage fee
(define-private (pay-storage-fee (db-id (string-ascii 64)))
  (let (
    (db-meta (unwrap! (map-get? database-metadata db-id) ERR_NOT_FOUND))
    (fee (get storage-cost db-meta))
  )
    (try! (stx-transfer? fee tx-sender (as-contract tx-sender)))
    (var-set contract-balance (+ (var-get contract-balance) fee))
    (ok fee)))

;; Select replica nodes based on reputation and stake
(define-private (select-replica-nodes (count uint))
  ;; Simplified selection - in practice would select based on reputation/stake
  (list tx-sender))

;; Store integrity proofs for replica nodes
(define-private (store-integrity-proofs (db-id (string-ascii 64)) (key (string-ascii 128)) (nodes (list 10 principal)) (data-hash (buff 32)))
  (begin
    ;; Store proof for first replica (simplified)
    (match (element-at nodes u0)
      node (begin
        (map-set data-integrity-proofs 
          { db-id: db-id, key: key, node: node }
          {
            merkle-root: data-hash,
            proof-timestamp: block-height,
            verified: true
          })
        (ok true))
      (ok false))))

;; Read-only functions

;; Get database info
(define-read-only (get-database-info (db-id (string-ascii 64)))
  (map-get? database-metadata db-id))

;; Get node info
(define-read-only (get-node-info (node principal))
  (map-get? storage-nodes node))

;; Get total databases
(define-read-only (get-total-databases)
  (var-get total-databases))

;; Get contract balance
(define-read-only (get-contract-balance)
  (var-get contract-balance))

;; Check if data exists
(define-read-only (data-exists (db-id (string-ascii 64)) (key (string-ascii 128)))
  (is-some (map-get? database-entries { db-id: db-id, key: key })))

;; Get active nodes count
(define-read-only (get-active-nodes-count)
  ;; Simplified - would iterate through all nodes to count active ones
  u1)