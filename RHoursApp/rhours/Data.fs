module RHours.Data

open System
open System.IO
open Microsoft.FSharp.Text.Lexing

open RHours.Crypto
open Json
open JsonSerialization

let mutable DoSave = true

type Project =
    {
        Id: string;
        Name: string;
    }

type ContributorInfoPublic =
    {
        PublicName: string;
        PublicKey: string;
        mutable PrivateInfoHash: byte array;
    }

type ContributorInfoAttribute =
    {
        Name: string;
        Value: string;
    }

type ContributorInfoPrivate = 
    {
        PublicKey: string;
        PrivateKey: string;
        Attributes: ContributorInfoAttribute list;
    }

type HourlyWithCompoundedInterestAndMaxTerm =
    {
        Hours: decimal;
        HourlyRate: decimal;
        Token: string;
        Interest: decimal;
        MaxMultiplier: decimal;
    }

type CashWithCompoundedInterestAndMaxTerm = 
    {
        Amount: decimal;
        Token: string;
        Interest: decimal;
        MaxMultiplier: decimal;
    }

type CompensationTerm =
    | HourlyWithCompoundedInterestAndMaxTerm of HourlyWithCompoundedInterestAndMaxTerm  // "hourly"
    | CashWithCompoundedInterestAndMaxTerm of CashWithCompoundedInterestAndMaxTerm      // "cash"
    | ExternalCompensation of string

type Contribution =
    {
        Terms: CompensationTerm;
        mutable Claims: string list;
    }

type ContributionSpan =
    {
        StartDate: DateTime;
        EndDate: DateTime;
        UtcOffset: float;
        mutable Contributions: Contribution list;
    }

type CompensationInvoice =
    {
        InvoiceId: string;
        Project: Project;
        Contributor: ContributorInfoPublic;
        mutable ContributionSpans: ContributionSpan list;
    }

type CompensationProposal =
    {
        Invoice: CompensationInvoice;
        mutable InvoiceHash: byte[];
        mutable ContributorSignature: byte[];       // Signature of InvoiceHash
        mutable ContributorPublicKey: string;
    }

type CompensationAgreement = 
    {
        Proposal: CompensationProposal;
        mutable ProposalHash: byte[];
        mutable ApproverSignature: byte[];      // Signature of ProposalHash using Approver Key
        mutable ApproverPublicKey: string;
    }

type Payment =
    {
        InvoiceId: string;
        Ammount: decimal;
    }

type CompensationSale =
    {
        InvoiceId: string;                                      // the invoice that is being sold
        CompensationAgreements : CompensationAgreement list;    // the compensation agreements which superceed the invoice
    }

type SaleProposal =
    {
        Sale: CompensationSale;
        SaleHash: byte[];                                       // Hash of CompensationSale
        SellorSignature: byte[];                                // Signature of SaleHash
        SellorPublicKey: string;
    }

type SaleAgreement =
    {
        Proposal: SaleProposal;
        ProposalHash: byte[];
        PurchasorSignature: byte[];        
        PurchasorPublicKey: string; 
    }

type IvoiceEvent =
    | Payment of Payment                // Happens when revenue is transferred to contributor
    | Sale of SaleAgreement             // Record of sale of contribution
    | Closure of InvoiceId: string      // Marker of fully compensated agreement

type RHoursConfig = 
    {
        PublicFolder: DirectoryInfo;
        PrivateFolder: DirectoryInfo;
    }

let ParseJsonFromString(json:string) =
    let lexbuf = LexBuffer<char>.FromString json
    JsonParser.start JsonLexer.json lexbuf
    
let ParseJsonFromFile (fileName:string) = 
    use textReader = new System.IO.StreamReader(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader textReader
    JsonParser.start JsonLexer.json lexbuf

type RHoursData =
    {
        Version: string;
        [<SkipSerialization>]
        mutable Config: RHoursConfig;
        mutable Projects : Project list;
        mutable Contributors : ContributorInfoPublic list;
        mutable CompensationAgreements : CompensationAgreement list;
        mutable InvoiceEvents : IvoiceEvent list;
    } with

    member this.Initialize(rhoursConfig: RHoursConfig option) =
        // Create the public folder if it doesn't exist
        // Create the private folder if it doesn't exist
        // Parse and desrialize the data in the rhours.json file if it exists
        // Save the current data to an rhours.json file
        //      This will create it if it didn't exist before
        //      Also saves it in the standard format if it wasn't before

        match rhoursConfig with
        | Some(config) -> 
            this.Config <- config
        | None ->
            let curdir = DirectoryInfo(Directory.GetCurrentDirectory())
            let privatedir = DirectoryInfo(Path.Combine(curdir.Parent.FullName, sprintf "%s_private" (curdir.Name)))

            this.Config <-
                {
                    PublicFolder = curdir;
                    PrivateFolder = privatedir;
                }

        if not(this.Config.PublicFolder.Exists) then
            this.Config.PublicFolder.Create()        

        if not(this.Config.PrivateFolder.Exists) then
            this.Config.PrivateFolder.Create()
        
        let files = this.Config.PublicFolder.GetFiles("rhours.json")
        if files.Length = 1 then
            let fileJson = ParseJsonFromFile (files.[0].FullName)
            let fileData = JsonSerialization.Deserialize<RHoursData> fileJson
            this.Projects <- fileData.Projects
            this.Contributors <- fileData.Contributors
            this.CompensationAgreements <- fileData.CompensationAgreements
            this.InvoiceEvents <- fileData.InvoiceEvents
        
        this.Save()

    member this.Save() =
        let json = Serialize this
        if DoSave then
            use rhoursFile = File.CreateText(Path.Combine(this.Config.PublicFolder.FullName, "rhours.json"))
            WriteJsonIndented rhoursFile json
        
    member this.ProjectExists(id: string) = 
        (this.Projects) |> List.exists (fun x -> x.Id = id)

    member this.ContributorExists(publicName: string) = 
        (this.Contributors) |> List.exists (fun x -> x.PublicName = publicName)

    member this.InvoiceExists(invoiceId: string) =
        this.CompensationAgreements 
            |> List.exists (
                fun a ->
                    a.Proposal.Invoice.InvoiceId = invoiceId
                )

    member this.ContributorHasContributions(publicName: string) =
        this.CompensationAgreements 
            |> List.exists ( 
                fun a -> 
                    a.Proposal.Invoice.Contributor.PublicName = publicName
                )

    member this.ProjectHasContributions(projectId: string) =
        this.CompensationAgreements
            |> List.exists ( 
                fun a ->
                    a.Proposal.Invoice.Project.Id = projectId
                )
    
    member this.AddProject (id: string, name: string) : Choice<Project, string> =
        match this.ProjectExists(id) with
        | false -> 
            let project = { Project.Id = id; Name = name}
            this.Projects <- project :: (this.Projects)
            Choice1Of2(project)
        | true ->
            Choice2Of2(sprintf "A project with the id '%s' already exists." id)

    member this.GetProject (id: string) : Project =
        (this.Projects) |> List.find (fun x -> x.Id = id)

    member this.DeleteProject (id: string) : string option =
        match (this.ProjectExists(id), this.ProjectHasContributions(id)) with
        | (true, false) -> 
            this.Projects <- (this.Projects) |> List.filter (fun x -> x.Id <> id)
            None
        | (false, _) ->
            Some(sprintf "No project with id '%s' exists." id)
        | (_, true) -> 
            Some(sprintf "The project with id '%s' has contributions and cannot be deleted." id)

    member this.ContributorPrivateFileExists(name: string) =
        let filename = name + ".json"
        let files = this.Config.PrivateFolder.GetFiles(filename)
        files.Length > 0

    member this.GetContributorPrivateInfo(name: string) =
        let filename = name + ".json"
        let files = this.Config.PrivateFolder.GetFiles(filename)
        if files.Length = 1 then
            // load the private info from the file, makes a json object
            // deserialize the json to a typed object
            // calculate the hash
            // update the public information with the hash
            let json = ParseJsonFromFile (files.[0].FullName)
            let privateInfo = JsonSerialization.Deserialize<ContributorInfoPrivate> json
            privateInfo
        else
            failwith "Contributor private file does not exist."

    member this.CreateContributorPrivateFile(name: string) =
        if this.ContributorPrivateFileExists(name) then
            failwith "Contributor file exists."
        else
            let filename = name + ".json"
            File.CreateText(Path.Combine(this.Config.PrivateFolder.FullName, filename))
     
    member this.AddContributor (publicName: string) : Choice<ContributorInfoPublic, string> =
        match (this.ContributorExists(publicName), this.ContributorPrivateFileExists(publicName)) with
        | (false, false) -> 
            // generate key pair
            // create private key file
            // create ContributorInfoPrivate and serialize it to the file
            // need json with indent

            let (publicKey, privateKey) = CryptoProvider.CreateKeyPair()
            use privateInfoFile = this.CreateContributorPrivateFile(publicName)
            let privateInfo = 
                {
                    PublicKey = publicKey;
                    PrivateKey = privateKey;
                    Attributes = [ { ContributorInfoAttribute.Name = "Example Attribute Name"; Value = "Example Attribute Value"; }; ];
                }
            let json = Serialize privateInfo
            WriteJsonIndented privateInfoFile json

            let jsonBytes = GetJsonBytes json
            let privateInfoHash = CryptoProvider.Hash(jsonBytes)

            let publicInfo = { ContributorInfoPublic.PublicName = publicName; PublicKey = publicKey; PrivateInfoHash = privateInfoHash; }
            this.Contributors <- publicInfo :: (this.Contributors)
            Choice1Of2(publicInfo)
        | (true, _) ->
            Choice2Of2(sprintf "A contributor with the name '%s' already exists." publicName)
        | (_, true) ->
            Choice2Of2("A contributor private file already exists.")

    member this.GetContributor (publicName: string) : ContributorInfoPublic =
        (this.Contributors) |> List.find (fun x -> x.PublicName = publicName)

    member this.HashContributor (name: string) : string option =
        match this.ContributorExists(name) with
        | true -> 
            let privateInfo = this.GetContributorPrivateInfo(name)
            let serializedJson = JsonSerialization.Serialize privateInfo
            let jsonBytes = GetJsonBytes serializedJson
            let privateInfoHash = CryptoProvider.Hash(jsonBytes)
            let publicInfo = this.GetContributor(name)
            publicInfo.PrivateInfoHash <- privateInfoHash
            None
        | false ->
            Some(sprintf "No contributor with name '%s' exists." name)

    member this.DeleteContributor (publicName: string) : string option =
        match (this.ContributorExists(publicName), this.ContributorHasContributions(publicName)) with
        | (true, false) -> 
            this.Contributors <- (this.Contributors) |> List.filter (fun x -> x.PublicName <> publicName)
            None
        | (false, _) ->
            Some(sprintf "No contributor with name '%s' exists." publicName)
        | (_, true) ->
            Some(sprintf "The contributor with name '%s' has contributions and cannot be deleted." publicName)

    member this.AddAgreement (project: Project, contributor: ContributorInfoPublic) : CompensationAgreement = 
        // Create a new InvoiceId
        // Create an Agreement with empty keys and hashes
        let invoiceId = CryptoProvider.RandomId()
        let agreement = 
            {
                Proposal = 
                    {
                        Invoice = 
                            {
                                InvoiceId = invoiceId;
                                Project = project;
                                Contributor = contributor;
                                ContributionSpans = [];
                            };
                        InvoiceHash = [| |];
                        ContributorSignature = [| |];
                        ContributorPublicKey = String.Empty;
                    };
                ProposalHash = [| |];
                ApproverSignature = [| |];
                ApproverPublicKey = String.Empty;
            }
        this.CompensationAgreements <- agreement :: (this.CompensationAgreements)
        agreement
    
    member this.GetAgreement(invoiceId: string) =
        (this.CompensationAgreements) |> List.find (fun x -> x.Proposal.Invoice.InvoiceId = invoiceId)
    
    member this.AddSpan(agreement: CompensationAgreement, startDate: DateTime, endDate: DateTime, utcOffset: float) : Choice<ContributionSpan, string> =
        if startDate > endDate then
            Choice2Of2("The start date of a contribution be before its end date.")
        else
            let span = 
                {
                    StartDate = startDate;
                    EndDate = endDate;
                    UtcOffset = utcOffset;
                    Contributions = [];
                }
            
            agreement.Proposal.Invoice.ContributionSpans <- span :: (agreement.Proposal.Invoice.ContributionSpans)
            Choice1Of2(span)

    member this.AddContribution(span: ContributionSpan, terms: CompensationTerm) : Contribution =
        let contribution = 
            {
                Terms = terms;
                Claims = [];
            }
        span.Contributions <- contribution :: (span.Contributions)
        contribution

    member this.AddClaims(contribution: Contribution, claim: string) =
        contribution.Claims <- claim :: (contribution.Claims)

    member this.CreateHourlyWithCompoundedInterestAndMaxTerm(hours: decimal, rate: decimal, token: string, interest: decimal, maxmult: decimal) : Choice<HourlyWithCompoundedInterestAndMaxTerm, string> =
        if hours <= 0m then
            Choice2Of2("Hours must be greater than zero.")
        elif rate <= 0m then
            Choice2Of2("Rate must be greater than zero.")
        elif interest <= 0m then
            Choice2Of2("Interest must be greater than zero.")
        elif maxmult <= 1m then
            Choice2Of2("Max Multiplier must be greater than one.")
        else
            let term = 
                {
                    Hours = hours;
                    HourlyRate = rate;
                    Token = token;
                    Interest = interest;
                    MaxMultiplier = maxmult;
                }
            Choice1Of2(term)

    member this.CreateCashWithCompoundedInterestAndMaxTerm(amount: decimal, token: string, interest: decimal, maxmult: decimal) : Choice<CashWithCompoundedInterestAndMaxTerm, string> =
        if amount <= 0m then
            Choice2Of2("Amount must be greater than zero.")
        elif interest <= 0m then
            Choice2Of2("Interest must be greater than zero.")
        elif maxmult <= 1m then
            Choice2Of2("Max Multiplier must be greater than one.")
        else
            let term =
                {
                    Amount = amount;
                    Token = token;
                    Interest = interest;
                    MaxMultiplier = maxmult;
                }
            Choice1Of2(term)

    member this.SignProposal(proposal: CompensationProposal, contributorPublicName: string) : Choice<string, string> = 
        // if this proposal has a non-empty ContributorSignature then error
        // if the contributor public name doesn't exist, then error
        // Calculate an InvoiceHash
        // Sign the invoice hash with the private key of the contributor public name
        // Generate a file in the private folder with the name {invoiceId}_proposal.json

        match proposal.ContributorSignature with
        | [| |] -> 
            if this.ContributorExists(contributorPublicName) then
                let privateInfo = this.GetContributorPrivateInfo(contributorPublicName)

                let jsonInvoice = Serialize (proposal.Invoice)
                let jsonBytes = GetJsonBytes jsonInvoice
                proposal.InvoiceHash <- CryptoProvider.Hash(jsonBytes)
                proposal.ContributorSignature <- CryptoProvider.Sign(privateInfo.PrivateKey, proposal.InvoiceHash)
                proposal.ContributorPublicKey <- privateInfo.PublicKey

                let jsonProposal = Serialize proposal
                let filename = sprintf "%s_proposal.json" (proposal.Invoice.InvoiceId)
                use proposalFile = File.CreateText(Path.Combine(this.Config.PrivateFolder.FullName, filename))
                WriteJsonIndented proposalFile jsonProposal
                Choice1Of2(filename)
            else
                Choice2Of2(sprintf "Contributor with the public name '%s' does not exist." contributorPublicName)
        | _ ->
            Choice2Of2("Cannot sign an invoice that is already signed.")

    member this.SignAgreement(agreement: CompensationAgreement, approverPublicName: string) : Choice<string, string> =
        // if this agreement has a non-empty ApproverSignature then error
        // if the approver public name doesn't exist, then error
        // Calculate a ProposalHash
        // Sign the proposal hash with the private key of the approver public name
        // Generate a file in the private folder with the name {invoiceId}_agreement.json

        match agreement.ApproverSignature with
        | [| |] -> 
            if this.ContributorExists(approverPublicName) then
                let privateInfo = this.GetContributorPrivateInfo(approverPublicName)

                let jsonProposal = Serialize (agreement.Proposal)
                let jsonBytes = GetJsonBytes jsonProposal
                agreement.ProposalHash <- CryptoProvider.Hash(jsonBytes)
                agreement.ApproverSignature <- CryptoProvider.Sign(privateInfo.PrivateKey, agreement.ProposalHash)
                agreement.ApproverPublicKey <- privateInfo.PublicKey

                let jsonAgreement = Serialize agreement
                let filename = sprintf "%s_agreement.json" (agreement.Proposal.Invoice.InvoiceId)
                use agreementFile = File.CreateText(Path.Combine(this.Config.PrivateFolder.FullName, filename))
                WriteJsonIndented agreementFile jsonAgreement
                Choice1Of2(filename)
            else
                Choice2Of2(sprintf "Contributor with the public name '%s' does not exist." approverPublicName)
        | _ ->
            Choice2Of2("Cannot sign an invoice that is already signed.")
        
    