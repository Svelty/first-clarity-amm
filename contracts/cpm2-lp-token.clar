;; title: cpm2-lp
;; version:
;; summary:
;; description:

;; unsure if this should be its own contract or be part of the amm itself...
;; 

;; traits
(impl-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard.sip-010-trait)

;; token definitions
(define-fungible-token cpm2-lp)

;; constants
(define-constant contract-owner .constant-product-amm2)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))

;; data vars
;;

;; data maps
;;

;; public functions
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-token-owner)
        (try! (ft-transfer? cpm2-lp amount sender recipient))
        (match memo to-print (print to-print) 0x)
        (ok true)
    )
)

(define-public (mint (amount uint) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ft-mint? cpm2-lp amount recipient)
    )
)

(define-public (burn (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ft-burn? cpm2-lp amount tx-sender) ;;can only burn tokens held by owner
    )
)

;; read only functions
(define-read-only (get-name)
    (ok "Constant Product AMM Two LP")
)

(define-read-only (get-symbol)
    (ok "CPMT-LP")
)

(define-read-only (get-decimals)
    (ok u6)
)

(define-read-only (get-balance (who principal))
    (ok (ft-get-balance cpm2-lp who))
)

(define-read-only (get-total-supply)
    (ok (ft-get-supply cpm2-lp))
)

(define-read-only (get-token-uri)
    (ok none)
)