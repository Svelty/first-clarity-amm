;; title: cpm-lp
;; version:
;; summary:
;; description:

;; unsure if this should be its own contract or be part of the amm itself...
;; 

;; traits
;; (impl-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard.sip-010-trait)
;; TODO: update contract to implement:
(impl-trait 'SPDBEG5X8XD50SPM1JJH0E5CTXGDV5NJTKAKKR5V.sip013-semi-fungible-token-trait.sip013-semi-fungible-token-trait)

;; token definitions
(define-fungible-token cpm-lp)

;; constants
(define-constant contract-owner .constant-product-amm)
(define-constant err-owner-only (err u1000))
(define-constant err-not-token-owner (err u1001))
(define-constant err-token-does-not-exist (err u1002))
(define-constant err-token-total-supply-not-found (err u1003))
(define-constant err-token-already-exists (err u1004))

;; data vars
(define-data-var token-id-nonce uint u0)

;; data maps
;; TODO: is pair-id needed?
(define-map tokens uint { pair-id: uint, total-supply: uint, token1:principal, token2: principal })
(define-map token-ids { token1: principal, token2: principal } uint)
(define-map balances { address: principal, token-id: uint } uint) ;;no way to iterate though addresses that own a given lp token

;; public functions
(define-public (register-new-token (pair-id uint) (token1 principal) (token2 principal))
    (let
        (
            (token-id (var-get token-id-nonce))
        )
        (asserts! (is-owner) err-owner-only)
        (asserts! (is-none (map-get? token-ids { token1: token1, token2: token2 })) err-token-already-exists)
        (asserts! (is-none (map-get? token-ids { token1: token2, token2: token1 })) err-token-already-exists)
        (map-set tokens token-id { pair-id: pair-id, total-supply: u0, token1: token1, token2: token2 })
        (map-set token-ids { token1: token1, token2: token2 } token-id)
        (var-set token-id-nonce (+ token-id u1))
        (ok token-id)
    )
)

(define-public (mint (amount uint) (token-id uint) (recipient principal))
    (let 
        (
            (token (unwrap! (map-get? tokens token-id) err-token-does-not-exist))
            (token-supply (get total-supply token))
        )
        (asserts! (is-owner) err-owner-only)
        (map-set balances 
            { address: recipient, token-id: token-id} 
            (+ (default-to u0 (map-get? balances { address: recipient, token-id: token-id })) amount)
        )
        (map-set tokens token-id (merge token { total-supply: (+ token-supply amount)}))
        (ft-mint? cpm-lp amount recipient)
    )
)

(define-public (burn (amount uint) (token-id uint) (recipient principal))
    (let
        (
            (token (unwrap! (map-get? tokens token-id) err-token-does-not-exist))
            (token-supply (get total-supply token))
        )
        (asserts! (is-owner) err-owner-only)
        (map-set balances 
            { address: recipient, token-id: token-id} 
            (- (default-to u0 (map-get? balances { address: recipient, token-id: token-id })) amount)
        )
        (map-set tokens token-id (merge token { total-supply: (- token-supply amount)}))
        (ft-burn? cpm-lp amount recipient)
    )
)

(define-public (transfer (amount uint) (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-token-owner)
        (try! (ft-transfer? cpm-lp amount sender recipient))
        (map-set balances 
            { address: sender, token-id: token-id} 
            (- (default-to u0 (map-get? balances { address: sender, token-id: token-id })) amount)
        )
        (map-set balances 
            { address: recipient, token-id: token-id} 
            (+ (default-to u0 (map-get? balances { address: recipient, token-id: token-id })) amount)
        )
        (ok true)
    )
)

(define-public (transfer-memo (amount uint) (token-id uint) (sender principal) (recipient principal) (memo (buff 34)))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-token-owner)
        (try! (ft-transfer? cpm-lp amount sender recipient))
        (map-set balances 
            { address: sender, token-id: token-id} 
            (- (default-to u0 (map-get? balances { address: sender, token-id: token-id })) amount)
        )
        (map-set balances 
            { address: recipient, token-id: token-id} 
            (+ (default-to u0 (map-get? balances { address: recipient, token-id: token-id })) amount)
        )
        (print memo)
        (ok true)
    )
)



;; read only functions
(define-read-only (get-name)
    (ok "Constant Product AMM LP")
)

(define-read-only (get-symbol)
    (ok "CPM-LP")
)

(define-read-only (get-decimals (token-id uint))
    (ok u6)
)

(define-read-only (get-balance (token-id uint) (who principal))
    (ok (default-to u0 (map-get? balances { address: who, token-id: token-id })))
)

(define-read-only (get-overall-balance (who principal))
    (ok (ft-get-balance cpm-lp who))
)

(define-read-only (get-total-supply (token-id uint))
    (ok (default-to u0 (get total-supply (map-get? tokens token-id))))
)

(define-read-only (get-overall-supply)
    (ok (ft-get-supply cpm-lp))
)

(define-read-only (get-token-uri (token-id uint))
    (ok none)
)

(define-read-only (is-owner)
    (is-eq tx-sender contract-owner)
)