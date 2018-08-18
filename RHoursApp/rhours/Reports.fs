module RHours.Reports

open RHours.Data
open System

let private PrettyDateTime (dt: DateTime) : string =
    if dt.Hour + dt.Minute + dt.Second + dt.Millisecond = 0 then
        dt.ToString("yyyy-MM-dd")
    else
        dt.ToLongDateString()

let private PrettyStringArray (a: string list) : string =
    match a.Length with
    | 0 -> ""
    | 1 -> a.[0]
    | n -> 
        let mutable s = ""
        for i = 0 to n-2 do
            s <- s + (a.[i]) + "; "
        s + a.[n-1]

let private PrettyContribution (term: CompensationTerm) : string =
    match term with
    | CompensationTerm.HourlyWithCompoundedInterestAndMaxTerm(t) -> // "hourly"
        sprintf "%.1f Hours" (t.Hours)
    | CompensationTerm.CashWithCompoundedInterestAndMaxTerm(t) ->   // "cash"
        sprintf "%.2f %s" (t.Amount) (t.Token)
    | CompensationTerm.ExternalCompensation(s) -> s

let private ContributionDetail (detail: DateTime * DateTime * string * string * string list) =
    let (spanBegin, spanEnd, contributor, amount, claims) = detail

    printfn "%s | %s | %s | %s | %s" (PrettyDateTime spanBegin) (PrettyDateTime spanEnd) contributor amount (PrettyStringArray claims)

let private ContributionProject (data: RHoursData) (project: Project) =
    printfn "## Project: %s" (project.Name)
    printfn ""

    printfn "Begin | End | Contributor | Amount | Claims"
    printfn "----- | --- | ----------- | ------ | ------"

    // CompensationAgreement  (list)
    //   .Proposal
    //      .Invoice
    //         .Project
    //         .Contributor
    //         .ContributionSpans  (list)
    //             .StartDate
    //             .EndDate
    //             .Contributions   (list)
    //                 .Terms
    //                      | HourlyWithCompoundedInterestAndMaxTerm of HourlyWithCompoundedInterestAndMaxTerm  // "hourly"
    //                      | CashWithCompoundedInterestAndMaxTerm of CashWithCompoundedInterestAndMaxTerm      // "cash"
    //                      | ExternalCompensation of string
    //                 .Claims

    let details = 
        seq {
            for agreement in data.CompensationAgreements do
                if agreement.Proposal.Invoice.Project.Id = project.Id then
                    for span in (agreement.Proposal.Invoice.ContributionSpans) |> Seq.sortByDescending (fun s -> s.StartDate) do
                        for contribution in span.Contributions do
                            yield (
                                (span.StartDate),
                                (span.EndDate),
                                (agreement.Proposal.Invoice.Contributor.PublicName),
                                (PrettyContribution (contribution.Terms)),
                                (contribution.Claims))
        }
    
    details |> Seq.iter ContributionDetail
       

let ContributionReport (data: RHoursData) =
    printfn "# Contributions"
    printfn ""

    (data.Projects) |> List.iter (ContributionProject data)

(*

# Contributions

## Project: RHours

Begin | End | Contributor | Amount | Claims
----- | --- | ----------- | ------ | ------
2018-07-11 | 2018-07-12 | Jake-Gillberg | 150.00 USD | Paid friend for RHours logo.
2018-07-04 | 2018-07-05 | JoshOrndorff | 1.0 Hours | Discussed economics, branding, implementation; Meeting with Jake and Glen.


*)
