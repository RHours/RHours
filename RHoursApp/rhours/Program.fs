
open System.IO

open RHours.Commands
open RHours.Data

let test () =

    File.Delete(@"C:\Projects\RChain\RHours\glenbraun\RHours\rhours.json")
    (Directory.GetFiles(@"C:\Projects\RChain\RHours\glenbraun\Rhours_private"))
        |> Array.iter (fun f -> File.Delete(f))

    let data = 
        {
            Version = "1.0";
            Config = 
                { 
                    PublicFolder = new DirectoryInfo("..\\..\\..\\..\\..\\");
                    PrivateFolder = new DirectoryInfo("..\\..\\..\\..\\..\\..\\RHours_private");
                };

            Projects = 
                [
                    {
                        Project.Id = "rhours";
                        Name = "RHours";
                    };
                ];
            Contributors = 
                [
                    {
                        ContributorInfoPublic.PublicName = "glen";
                        PublicKey = "MCowBQYDK2VwAyEACOnA0dtn/SPVrl/OVYE1//xZP0xGV7x2vxjkgFH0cW0=";
                        PrivateInfoHash = [| |];
                    };
                    {
                        ContributorInfoPublic.PublicName = "jake";
                        PublicKey = "Jake Public";
                        PrivateInfoHash = [| |];
                    };
                    {
                        ContributorInfoPublic.PublicName = "joshy";
                        PublicKey = "MCowBQYDK2VwAyEAedqYOtnLSAkDXDg4+ovGow+HA1KZmM5SsuaKJJD6Xf8=";
                        PrivateInfoHash = [| |];
                    };
                ];
            CompensationAgreements = [];
            InvoiceEvents = [];
        }

    RHours.Data.DoSave <- false
    RHours.Commands.Data <- data
    RHours.Commands.Data.Initialize(Some(data.Config))
    
    RunRHoursMenu()

[<EntryPoint>]
let main argv =
    // If rhours.json does not exist
    // Create a config with the current directory and 

    let configRHours =
        {
            PublicFolder = DirectoryInfo(@"C:\Projects\RHours\RHours");
            PrivateFolder = DirectoryInfo(@"C:\Projects\RHours\RHours_private");
        }

    let configDocs =
        {
            PublicFolder = DirectoryInfo(@"C:\Projects\RHours\RChain_Documentation");
            PrivateFolder = DirectoryInfo(@"C:\Projects\RHours\RChain_Documentation_private");
        }

    let configRCoopProcess = 
        {
            PublicFolder = DirectoryInfo(@"C:\Projects\RHours\RCoopProcess");
            PrivateFolder = DirectoryInfo(@"C:\Projects\RHours\RCoopProcess_private");
        }

    let configRCLRHours = 
        {
            PublicFolder = DirectoryInfo(@"C:\Projects\RHours\RCL-RHours");
            PrivateFolder = DirectoryInfo(@"C:\Projects\RHours\Private\RCL-RHours");
        }

    
    RHours.Commands.Data.Initialize(Some(configRCLRHours))

    RunRHoursMenu()

    0 // return an integer exit code

(*


project add rhours RHours
contributor add glenbraun
contributor add JoshOrndorff
contributor add Jake-Gillberg
contributor select Jake-Gillberg
project select rhours
contributor select Jake-Gillberg
invoice
add
span
add 2018-07-03 2018-07-04 -5
contribution
add cash 135 USD 1 10
claim add "Paid a friend to make an RHours logo"
back
back
contributor select JoshOrndorff
invoice
add
span
add 2018-07-04 2018-07-05 -4
contribution
add hourly 1 100 USD 1 10
claim add "Meeting with Jake and Glen."
claim add "Discussed economics, branding, implementation."
back
span
add 2018-07-11 2018-07-12 -4
contribution
add hourly 1.5 100 USD 1 10
claim add "Paid friend for RHours logo."


*)
(*
            ContributionSpans = 
                [
                    {
                        ProjectId = "rhours";
                        ContributorId = "joshy";
                        StartDate = DateTime(2018, 7, 4);
                        EndDate = DateTime(2018, 7, 5);
                        UtcOffset = 4.0;
                        Contributions = 
                            [
                                {
                                    Contribution.Id = "1";
                                    Terms = CompensationTerm.HourlyWithCompoundedInterestAndMaxTerm
                                                (
                                                    {
                                                        Hours = 1.0m;
                                                        HourlyRate = 100.0m
                                                        Token = "USD";
                                                        Interest = 1.0m;
                                                        MaxMultiplier = 10m;
                                                    }
                                                );
                                    Claims = ["Meeting with Jake and Glen."; "Discussed economics, branding, implementation.";];
                                };
                                {
                                    Contribution.Id = "2";
                                    Terms = CompensationTerm.HourlyWithCompoundedInterestAndMaxTerm
                                                (
                                                    {
                                                        Hours = 1.0m;
                                                        HourlyRate = 100.0m
                                                        Token = "USD";
                                                        Interest = 1.0m;
                                                        MaxMultiplier = 10m;
                                                    }
                                                );
                                    Claims = ["Coding on solidity contract";];
                                }
                            ];
                    };
                    {
                        ProjectId = "rhours";
                        ContributorId = "joshy";
                        StartDate = DateTime(2018, 7, 11);
                        EndDate = DateTime(2018, 7, 12);
                        UtcOffset = 5.0;
                        Contributions = 
                            [
                                {
                                    Contribution.Id = "1";
                                    Terms = CompensationTerm.HourlyWithCompoundedInterestAndMaxTerm
                                                (
                                                    {
                                                        Hours = 1.5m;
                                                        HourlyRate = 100.0m
                                                        Token = "USD";
                                                        Interest = 1.0m;
                                                        MaxMultiplier = 10m;
                                                    }
                                                );
                                    Claims = ["Meeting with Jake and Glen."; "Discussed ideology, law, abstraction.";];
                                }
                            ];
                    };
                    {
                        ProjectId = "rhours";
                        ContributorId = "jake";
                        StartDate = DateTime(2018, 7, 3);
                        EndDate = DateTime(2018, 7, 4);
                        UtcOffset = 7.0;
                        Contributions = 
                            [
                                {
                                    Contribution.Id = "1";
                                    Terms = CompensationTerm.CashWithCompoundedInterestAndMaxTerm
                                                (
                                                    {
                                                        Amount = 135.0m;
                                                        Token = "USD";
                                                        Interest = 1.0m;
                                                        MaxMultiplier = 10m;
                                                    }
                                                );
                                    Claims = ["Paid friend for RHours logo.";];
                                }
                            ];
                    };
                ];
*)
