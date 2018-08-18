module RHours.Commands

open System
open System.IO
open RHours.Data
open RHours.Reports

let mutable Data = 
    {
        Version = "1.0";
        Config = 
            { 
                PublicFolder = new DirectoryInfo(Directory.GetCurrentDirectory())
                PrivateFolder = new DirectoryInfo(Directory.GetCurrentDirectory())
            };
        Projects = [];
        Contributors = [];
        CompensationAgreements = [];
        InvoiceEvents = [];
    }

type CommandDefinition =
    {
        CommandText : string;
        HelpText: string;
        Execute: (string list -> unit)
    }

type CommandState =
    {
        mutable Name: string;
        mutable Defs: CommandDefinition list;
        mutable PreviousName: string;
        mutable PreviousDefs: CommandDefinition list;
        mutable ActiveProject: Project option;
        mutable ActiveContributor: ContributorInfoPublic option;
        mutable ActiveAgreement: CompensationAgreement option;
        mutable ActiveSpan: ContributionSpan option;
        mutable ActiveContribution: Contribution option;
    } with

    member this.SetToPrevious() =
        this.Name <- this.PreviousName
        this.Defs <- this.PreviousDefs

let internal SplitLine (line:string) =
    let rec split i s (words: string list) =
        if i >= line.Length then
            words
        else
            // states
            // 0 = not in word
            // 1 = in word that is not a quoted string
            // 2 = in quoted string
            match line.[i] with
            | '\"' when s <> 1 -> 
                match s with
                | 2 -> // in quoted string
                    split (i+1) 0 words
                | _ -> // not in word, in word that is not a quoted string
                    split (i+1) 2 ("" :: words)
            | ' ' when s <> 2 ->  // not in quoted string
                split (i+1) 0 words
            | c ->
                let newS = 
                    match s with
                    | 0 -> 1
                    | _ -> s

                match words with
                | [] -> 
                    split (i+1) newS [ String([| c; |]); ]
                | h :: t ->
                    match s with
                    | 0 -> 
                        split (i+1) newS ((String([| c; |])) :: h :: t)
                    | _ ->
                        let newH = h + String([| c; |])
                        split (i+1) newS (newH :: t)

    let wordlist = split 0 0 [] |> List.rev
    wordlist


let rec internal ParseLine (state: CommandState) (parts: string list) =
    match parts with
    | [ ] -> 
        ReadCommand state
    | h :: t -> 
        match ((state.Defs) |> List.tryFind (fun c -> c.CommandText = h)) with
        | Some(cmd) -> 
            (cmd.Execute) t
        | None ->
            printfn "Expected something else"
            ReadCommand state

and ReadCommand (state: CommandState) =
    printfn "%s Menu" (state.Name)
    printf "    "

    let length = (state.Defs) |> List.length
    (state.Defs) |> List.iteri (fun i cmd -> printf "%s%s" (cmd.CommandText) (if i < length - 1 then ", " else ""))
    printfn ""
    printf "> "

    state.PreviousName <- state.Name
    state.PreviousDefs <- state.Defs

    let line = Console.ReadLine()
    let parts = SplitLine line    
    ParseLine state parts
    Data.Save()
    
let internal state = 
    {
        Name = "Main";
        Defs = [];
        PreviousName = "Main";
        PreviousDefs = [];
        ActiveProject = None;
        ActiveContributor = None;
        ActiveAgreement = None;
        ActiveSpan = None;
        ActiveContribution = None;
    }

let rec internal ProjectMenu (parts: string list) =
    state.Name <- "Project"
    state.Defs <- 
        [
            {
                CommandText = "select";
                HelpText = "Select a project";
                Execute = ProjectSelectMenu;
            };
            {
                CommandText = "add";
                HelpText = "Add a project";
                Execute = ProjectAddMenu;
            };
            {
                CommandText = "delete";
                HelpText = "Delete a project";
                Execute = ProjectDeleteMenu;
            };
            {
                CommandText = "list";
                HelpText = "Shows the project list";
                Execute = ProjectListMenu;
            };
            {
                CommandText = "back";
                HelpText = "Moves back to the main menu";
                Execute = MainMenu;
            };
            {
                CommandText = "exit";
                HelpText = "Exit command";
                Execute = Exit;
            };
        ]
    
    ParseLine state parts

and ProjectSelectMenu (parts: string list) =
    printfn "Project Select %A" parts

    match parts with
    | [ id ] ->
        if Data.ProjectExists(id) then
            state.ActiveProject <- Some(Data.GetProject(id))
        else
            printfn "Project with id '%s' does not exist." id
    | _ ->
        printfn "Expected id"
    
    state.SetToPrevious()
    ReadCommand state

and ProjectAddMenu (parts: string list) =
    printfn "Project Add %A" parts
    match parts with
    | [ id; name] ->
        match (Data.AddProject(id, name)) with
        | Choice1Of2(project) -> state.ActiveProject <- Some(project)
        | Choice2Of2(err) -> printfn "%s" err
    | _ ->
        printfn "Expected id and name"

    state.SetToPrevious()
    ReadCommand state

and ProjectDeleteMenu (parts: string list) =
    printfn "Project Delete %A" parts

    match parts with
    | [ id ] ->
        match Data.DeleteProject(id) with
        | Some(err) -> printfn "%s" err
        | None -> ()
    | _ ->
        printfn "Expected id"

    state.SetToPrevious()
    ReadCommand state

and ProjectListMenu (parts: string list) =
    printfn "Projects: %A" Data.Projects
    state.SetToPrevious()
    ReadCommand state
    
and ContributorMenu (parts: string list) =
    state.Name <- "Contributor"
    state.Defs <- 
        [
            {
                CommandText = "add";
                HelpText = "Add a contributor";
                Execute = ContributorAddMenu;
            };
            {
                CommandText = "select";
                HelpText = "Select a contributor";
                Execute = ContributorSelectMenu;
            };
            {
                CommandText = "hash";
                HelpText = "Update hash of private info for a contributor."
                Execute = ContributorHashMenu;
            };
            {
                CommandText = "delete";
                HelpText = "Delete a contributor";
                Execute = ContributorDeleteMenu;
            };
            {
                CommandText = "list";
                HelpText = "Shows the contributor list";
                Execute = ContributorListMenu;
            };
            {
                CommandText = "back";
                HelpText = "Moves back to the main menu";
                Execute = MainMenu;
            };
            {
                CommandText = "exit";
                HelpText = "Exit command";
                Execute = Exit;
            };
        ]

    ParseLine state parts

and ContributorSelectMenu (parts: string list) =
    printfn "Contributor Select %A" parts

    match parts with
    | [ publicName ] ->
        if Data.ContributorExists(publicName) then
            state.ActiveContributor <- Some(Data.GetContributor(publicName))
        else
            printfn "Contributor with name '%s' does not exist." publicName
    | _ ->
        printfn "Expected public name"

    state.SetToPrevious()
    ReadCommand state

and ContributorAddMenu (parts: string list) =
    printfn "Contributor Add %A" parts
    match parts with
    | [ name; ] ->
        match (Data.AddContributor(name)) with
        | Choice1Of2(contributor) -> state.ActiveContributor <- Some(contributor)
        | Choice2Of2(err) -> printfn "%s" err
    | _ ->
        printfn "Expected name"

    state.SetToPrevious()
    ReadCommand state

and ContributorHashMenu (parts: string list) =
    printfn "Contributor Hash %A" parts

    match parts with
    | [ name ] ->
        match Data.HashContributor(name) with
        | Some(err) -> printfn "%s" err
        | None -> ()
    | _ ->
        printfn "Expected name"

    state.SetToPrevious()
    ReadCommand state

and ContributorDeleteMenu (parts: string list) =
    printfn "Contributor Delete %A" parts

    match parts with
    | [ name ] ->
        match Data.DeleteContributor(name) with
        | Some(err) -> printfn "%s" err
        | None -> ()
    | _ ->
        printfn "Expected name"

    state.SetToPrevious()
    ReadCommand state

and ContributorListMenu (parts: string list) =
    printfn "Contributors: %A" Data.Contributors
    state.SetToPrevious()
    ReadCommand state

and InvoiceMenu (parts: string list) =
    state.Name <- "Invoice Menu"
    state.Defs <- 
        [
            {
                CommandText = "add";
                HelpText = "Add a invoice";
                Execute = InvoiceAddMenu;
            };
            {
                CommandText = "list";
                HelpText = "List invoices";
                Execute = InvoiceListMenu;
            };
            {
                CommandText = "select";
                HelpText = "Select active invoice";
                Execute = InvoiceSelectMenu;
            };
            {
                CommandText = "span";
                HelpText = "Manage contribution spans.";
                Execute = InvoiceSpanMenu;
            };
            {
                CommandText = "proposal";
                HelpText = "Create a proposal signed by the given contributor.";
                Execute = InvoiceProposalMenu;
            };
            {
                CommandText = "agreement";
                HelpText = "Create an agreement signed by the given approver.";
                Execute = InvoiceAgreementMenu;
            };
            {
                CommandText = "back";
                HelpText = "Moves back to the main menu";
                Execute = MainMenu;
            };
            {
                CommandText = "exit";
                HelpText = "Exit command";
                Execute = Exit;
            };
        ]

    ParseLine state parts

and InvoiceListMenu (parts: string list) =
    printfn "Invoices: %A" Data.CompensationAgreements
    state.SetToPrevious()
    ReadCommand state

and InvoiceSelectMenu (parts: string list) =
    printfn "Invoice Select %A" parts

    match parts with
    | [ invoiceId ] ->
        if Data.InvoiceExists(invoiceId) then
            state.ActiveAgreement <- Some(Data.GetAgreement(invoiceId))
        else
            printfn "Invoice with id '%s' does not exist." invoiceId
    | _ ->
        printfn "Expected id"
    
    state.SetToPrevious()
    ReadCommand state

and InvoiceAddMenu (parts: string list) =
    printfn "Invoice Add %A" parts
    match (state.ActiveProject, state.ActiveContributor) with
    | (Some(project), Some(contributor)) ->
        match parts with
        | [ ] ->
            let agreement = Data.AddAgreement(project, contributor)
            state.ActiveAgreement <- Some(agreement)
        | _ ->
            printfn "Expected nothing"
    | (None, _) ->
        printfn "No active project selected"
    | (_, None) ->
        printfn "No active conributor selected"

    state.SetToPrevious()
    ReadCommand state

and InvoiceProposalMenu (parts: string list) =
    printfn "Invoice Proposal %A" parts
    match (state.ActiveAgreement) with
    | Some(agreement) ->
        match parts with
        | [ contributorPublicName ] ->
            match Data.SignProposal(agreement.Proposal, contributorPublicName) with
            | Choice1Of2(filename) -> 
                printfn "Created proposal file at: %s" filename
            | Choice2Of2(err) ->
                printfn "%s" err
        | _ ->
            printfn "Expected contributor public name"
    | None ->
        printfn "No active agreement selected"

    state.SetToPrevious()
    ReadCommand state

and InvoiceAgreementMenu (parts: string list) =
    printfn "Invoice Agreement %A" parts
    match (state.ActiveAgreement) with
    | Some(agreement) ->
        match parts with
        | [ approverPublicName ] ->
            match Data.SignAgreement(agreement, approverPublicName) with
            | Choice1Of2(filename) ->
                printfn "Created agreement file at: %s" filename
            | Choice2Of2(err) ->
                printfn "%s" err
        | _ ->
            printfn "Expected approver public name"
    | None ->
        printfn "No active agreement selected"

    state.SetToPrevious()
    ReadCommand state

and InvoiceSpanMenu (parts: string list) =
    state.Name <- "Invoice Span Menu"
    state.Defs <- 
        [
            {
                CommandText = "add";
                HelpText = "Add a span to the current invoice";
                Execute = InvoiceSpanAddMenu;
            };
            {
                CommandText = "list";
                HelpText = "List the spans for the active invoice.";
                Execute = InvoiceSpanListMenu;
            };
            {
                CommandText = "select";
                HelpText = "Select a span to edit.";
                Execute = InvoiceSpanSelectMenu;
            };
            {
                CommandText = "contribution";
                HelpText = "Contribution Menu.";
                Execute = ContributionMenu;
            };
            {
                CommandText = "back";
                HelpText = "Moves back to the invoice menu";
                Execute = InvoiceMenu;
            };
            {
                CommandText = "exit";
                HelpText = "Exit command";
                Execute = Exit;
            };
        ]

    ParseLine state parts

and InvoiceSpanAddMenu (parts: string list) =
    printfn "Invoice Span Add %A" parts
    match state.ActiveAgreement with
    | Some(agreement) ->
        match parts with
        | [ spanStart; spanEnd; spanOffset; ] ->
            let (startOK, startDate) = DateTime.TryParse(spanStart)
            let (endOK, endDate) = DateTime.TryParse(spanEnd)
            let (offsetOK, utcOffset) = Double.TryParse(spanOffset)

            match (startOK, endOK, offsetOK) with
            | (true, true, true) -> 
                match (Data.AddSpan(agreement, startDate, endDate, utcOffset)) with
                | Choice1Of2(span) -> 
                    state.ActiveSpan <- Some(span)
                | Choice2Of2(err) ->
                    printfn "%s" err
            | (false, _, _) ->
                printfn "Unable to parse StartDate"
            | (_, false, _) ->
                printfn "Unable to parse EndDate"
            | (_, _, false) ->
                printfn "Unable to parse UtcOffset"
        | _ ->
            printfn "Expected {StartDate} {EndDate} {UtcOffset}"
    | None ->
        printfn "No active agreement"

    state.SetToPrevious()
    ReadCommand state

and InvoiceSpanListMenu (parts: string list) =
    match (state.ActiveAgreement) with
    | Some(agreement) ->
        printfn "Spans: %A" (agreement.Proposal.Invoice.ContributionSpans)
        state.SetToPrevious()
        ReadCommand state
    | None ->
        printfn "No active agreement."

and InvoiceSpanSelectMenu (parts: string list) =
    printfn "Invoice Span Select %A" parts

    match (state.ActiveAgreement) with
    | Some(agreement) ->
        match parts with
        | [ spanIndex ] ->
            let (parseOK, index) = Int32.TryParse(spanIndex)
            if parseOK then
                if index >= 0 && index < (agreement.Proposal.Invoice.ContributionSpans.Length) then
                    // Choose the span, in reverse order with the given index
                    let span =
                        (agreement.Proposal.Invoice.ContributionSpans)
                            |> List.rev |> List.item index
                    state.ActiveSpan <- Some(span)
                else
                    printfn "Index out of range."
            else 
                printfn "Unable to parse index"
        | _ ->
            printfn "Expected {index}"
    | None ->
        printfn "No active agreement."
    
    state.SetToPrevious()
    ReadCommand state

and ContributionMenu (parts: string list) =
    state.Name <- "Contribution Menu"
    state.Defs <- 
        [
            {
                CommandText = "add";
                HelpText = "Add contribution to the current span";
                Execute = ContributionAddMenu;
            };
            {
                CommandText = "list";
                HelpText = "List the contributions for the active span.";
                Execute = ContributionListMenu;
            };
            {
                CommandText = "select";
                HelpText = "Select the contribution from the active span.";
                Execute = ContributionSelectMenu;
            };
            {
                CommandText = "claim";
                HelpText = "Claim Menu";
                Execute = ClaimMenu;
            };
            {
                CommandText = "back";
                HelpText = "Moves back to the invoice span menu";
                Execute = InvoiceSpanMenu;
            };
            {
                CommandText = "exit";
                HelpText = "Exit command";
                Execute = Exit;
            };
        ]

    ParseLine state parts

and ContributionAddMenu (parts: string list) =
    printfn "Contribution Add %A" parts
    match (state.ActiveSpan) with
    | Some(span) ->
        match parts with
        | [ "hourly"; sHours; sRate; token; sInterest; sMaxmult;] ->
            let (hoursOK, hours) = Decimal.TryParse(sHours)
            let (rateOK, rate) = Decimal.TryParse(sRate)
            let (interestOK, interest) = Decimal.TryParse(sInterest)
            let (maxmultOK, maxmult) = Decimal.TryParse(sMaxmult)

            match (hoursOK, rateOK, interestOK, maxmultOK) with
            | (true, true, true, true) ->
                let termsChoice = Data.CreateHourlyWithCompoundedInterestAndMaxTerm(hours, rate, token, interest, maxmult)
                match termsChoice with
                | Choice1Of2(terms) ->
                    let contribution = 
                        {
                            Terms = CompensationTerm.HourlyWithCompoundedInterestAndMaxTerm(terms);
                            Claims = [];
                        }
                    span.Contributions <- contribution :: (span.Contributions)
                    state.ActiveContribution <- Some(contribution)
                | Choice2Of2(err) ->
                    printfn "%s" err
            | (false, _, _, _) ->
                printfn "Unable to parse hours"
            | (_, false, _, _) ->
                printfn "Unable to parse rate"
            | (_, _, false, _) ->
                printfn "Unable to parse interest"
            | (_, _, _, false) ->
                printfn "Unable to parse maxmult"

        | [ "cash"; sAmount; token; sInterest; sMaxmult;] ->
            let (amountOK, amount) = Decimal.TryParse(sAmount)
            let (interestOK, interest) = Decimal.TryParse(sInterest)
            let (maxmultOK, maxmult) = Decimal.TryParse(sMaxmult)

            match (amountOK, interestOK, maxmultOK) with
            | (true, true, true) ->
                let termsChoice = Data.CreateCashWithCompoundedInterestAndMaxTerm(amount, token, interest, maxmult)
                match termsChoice with
                | Choice1Of2(terms) ->
                    let contribution = 
                        {
                            Terms = CompensationTerm.CashWithCompoundedInterestAndMaxTerm(terms);
                            Claims = [];
                        }
                    state.ActiveContribution <- Some(contribution)
                | Choice2Of2(err) ->
                    printfn "%s" err
            | (false, _, _) ->
                printfn "Unable to parse amount"
            | (_, false, _) ->
                printfn "Unable to parse interest"
            | (_, _, false) ->
                printfn "Unable to parse maxmult"
        | _ ->
            printfn "Expected nothing"
    | None ->
        printfn "No active span"

    state.SetToPrevious()
    ReadCommand state

and ContributionListMenu (parts: string list) =
    match (state.ActiveSpan) with
    | Some(span) ->
        printfn "Contributions: %A" (span.Contributions)
        state.SetToPrevious()
        ReadCommand state
    | None ->
        printfn "No active span."

and ContributionSelectMenu (parts: string list) =
    printfn "Contribution Select %A" parts

    match (state.ActiveSpan) with
    | Some(span) ->
        match parts with
        | [ contributionIndex ] ->
            let (parseOK, index) = Int32.TryParse(contributionIndex)
            if parseOK then
                if index >= 0 && index < (span.Contributions.Length) then
                    // Choose the contribution, in reverse order with the given index
                    let contribution =
                        (span.Contributions)
                            |> List.rev |> List.item index
                    state.ActiveContribution <- Some(contribution)
                else
                    printfn "Index out of range."
            else 
                printfn "Unable to parse index"
        | _ ->
            printfn "Expected {index}"
    | None ->
        printfn "No active span."
    
    state.SetToPrevious()
    ReadCommand state

and ClaimMenu (parts: string list) =
    state.Name <- "Claim Menu"
    state.Defs <- 
        [
            {
                CommandText = "add";
                HelpText = "Add claim to the current contribution";
                Execute = ClaimAddMenu;
            };
            {
                CommandText = "list";
                HelpText = "Lists the claims for the current contribution.";
                Execute = ClaimListMenu;
            };
            {
                CommandText = "back";
                HelpText = "Moves back to the contribution menu";
                Execute = ContributionMenu;
            };
            {
                CommandText = "exit";
                HelpText = "Exit command";
                Execute = Exit;
            };
        ]

    ParseLine state parts

and ClaimAddMenu (parts: string list) =
    printfn "Claim Add %A" parts

    match (state.ActiveContribution) with
    | Some(contribution) ->
        match parts with
        | [ claim; ] ->
            contribution.Claims <- claim :: (contribution.Claims)
        | _ ->
            printfn "Expected claim"

        state.SetToPrevious()
        ReadCommand state
    | None ->
        printfn "No active contribution"

and ClaimListMenu (parts: string list) =
    match (state.ActiveContribution) with
    | Some(contribution) ->
        printfn "Spans: %A" (contribution.Claims)
        state.SetToPrevious()
        ReadCommand state
    | None ->
        printfn "No active contribution."

and Exit (parts: string list) =
    printfn "%A" Data
    printfn "Exit"

and ReportMenu (parts: string list) =
    state.Name <- "Report"
    state.Defs <- 
        [
            {
                CommandText = "contributions";
                HelpText = "Run the contributions report";
                Execute = ReportContributionsMenu;
            };
            {
                CommandText = "back";
                HelpText = "Moves back to the main menu";
                Execute = MainMenu;
            };
            {
                CommandText = "exit";
                HelpText = "Exit command";
                Execute = Exit;
            };
        ]
    
    ParseLine state parts

and ReportContributionsMenu (parts: string list) =
    ContributionReport Data

    ReadCommand state

and MainMenu (parts: string list) =
    let subcommands = 
        [
            {
                CommandText = "invoice";
                HelpText = "Used to manage the invoices.";
                Execute = InvoiceMenu;
            };
            {
                CommandText = "contributor";
                HelpText = "Contributor command";
                Execute = ContributorMenu;
            };
            {
                CommandText = "project";
                HelpText = "Project command";
                Execute = ProjectMenu;
            };
            {
                CommandText = "report";
                HelpText = "Report command";
                Execute = ReportMenu;
            };
            {
                CommandText = "exit";
                HelpText = "Exit command";
                Execute = Exit;
            };
        ]

    state.Name <- "Main"
    state.Defs <- subcommands
    state.PreviousName <- "Main"
    state.PreviousDefs <- subcommands
    
    ReadCommand state

let RunRHoursMenu() =
    MainMenu []
