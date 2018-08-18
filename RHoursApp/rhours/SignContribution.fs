module SignContribution

open RHours.Data
open JsonSerialization

open RHours.Crypto

open System
open System.IO
open System.Text
open Json

// Done by the contributor
let SignCompensationInvoice (invoice: CompensationInvoice) (publicKey: string) (privateKey: string) = 
    //let (publicKeyText, privateKeyText) = Ed25519Signature.CreateKeyPair()

    let json = Serialize invoice
    let jsonBytes = GetJsonBytes json
    let hashBytes = CryptoProvider.Hash(jsonBytes)

    let pk = 
        if privateKey.StartsWith("-----") then 
            privateKey
        else
            sprintf "%s\n%s\n%s" "-----BEGIN PRIVATE KEY-----" privateKey "-----END PRIVATE KEY-----"

    let signedBytes = CryptoProvider.Sign(pk, hashBytes)

    let signedProposal = 
        {
            Invoice = invoice;
            InvoiceHash = hashBytes;
            ContributorSignature = signedBytes;
            ContributorPublicKey = publicKey;
        }

    signedProposal

// Done by the project leader
let SignCompensationProposal (proposal: CompensationProposal) (publicKey: string) (privateKey: string) = 
    let json = Serialize proposal
    let jsonBytes = GetJsonBytes json
    let hashBytes = CryptoProvider.Hash(jsonBytes)

    let pk = 
        if privateKey.StartsWith("-----") then 
            privateKey
        else
            sprintf "%s\n%s\n%s" "-----BEGIN PRIVATE KEY-----" privateKey "-----END PRIVATE KEY-----"

    let signedBytes = CryptoProvider.Sign(pk, hashBytes)

    let signedAgreement = 
        {
            Proposal = proposal;
            ProposalHash = hashBytes;           // Hash of Proposal
            ApproverSignature = signedBytes;    // Signature of AcceptanceHash using Acceptor Key
            ApproverPublicKey = publicKey;
        }

    signedAgreement

// Verify ContributorSignedCompensationAgreement
let VerifyCompensationProposal (proposal: CompensationProposal) =
    let pk = 
        if proposal.ContributorPublicKey.StartsWith("-----") then
            proposal.ContributorPublicKey
        else
            sprintf "%s\n%s\n%s" "-----BEGIN PUBLIC KEY-----" (proposal.ContributorPublicKey) "-----END PUBLIC KEY-----"
    
    let verified = CryptoProvider.Verify(pk, proposal.InvoiceHash, proposal.ContributorSignature)
    verified

// Verify AcceptedCompensationAgreement
let VerifyCompensationAgreement (agreement: CompensationAgreement) =
    let pk = 
        if agreement.ApproverPublicKey.StartsWith("-----") then
            agreement.ApproverPublicKey
        else
            sprintf "%s\n%s\n%s" "-----BEGIN PUBLIC KEY-----" (agreement.ApproverPublicKey) "-----END PUBLIC KEY-----"
    
    let verified = CryptoProvider.Verify(pk, agreement.ProposalHash, agreement.ApproverSignature)
    verified
