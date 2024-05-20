open System
open System.IO
open System.Timers
open System.Data
open System.Data.SQLite
open Donald

type Project = { name: string }

module Project =
    let ofDataReader (reader: IDataReader) = { name = reader.ReadString "name" }

type Task =
    { project: Project
      name: string
      time: int }

module Task =
    let ofDataReader (reader: IDataReader) =
        { name = reader.ReadString "name"
          project = { name = reader.ReadString "project" }
          time = reader.ReadInt32 "time" }


let connection =
    let path =
        Path.Combine(Environment.GetEnvironmentVariable("HOME"), "timetracker.sqlite")

    new SQLiteConnection($"Data Source={path};Version=3;Pooling=True;Max Pool Size=100;")

module Database =
    let createTables (connection: IDbConnection) =
        use transaction = connection.TryBeginTransaction()

        connection
        |> Db.newCommand "CREATE TABLE IF NOT EXISTS project (name TEXT PRIMARY KEY)"
        |> Db.setTransaction transaction
        |> Db.exec

        connection
        |> Db.newCommand
            "CREATE TABLE IF NOT EXISTS task (project TEXT, name TEXT, time INT NOT NULL DEFAULT 0, PRIMARY KEY (project, name), FOREIGN KEY (project) REFERENCES project(name))"
        |> Db.setTransaction transaction
        |> Db.exec

        transaction.Commit()

    let getProjects (connection: IDbConnection) =
        connection
        |> Db.newCommand "SELECT name FROM project"
        |> Db.query (fun r -> Project.ofDataReader r)
        |> List.toArray

    let createProject (connection: IDbConnection) (project: Project) =
        connection
        |> Db.newCommand "INSERT INTO project (name) VALUES (@name)"
        |> Db.setParams [ "@name", SqlType.String project.name ]
        |> Db.exec

    let getProjectTasks (connection: IDbConnection) (project: Project) =
        connection
        |> Db.newCommand "SELECT name, project, time FROM task WHERE project = @project"
        |> Db.setParams [ "@project", SqlType.String project.name ]
        |> Db.query (fun r -> Task.ofDataReader r)
        |> List.toArray

    let createProjectTask (connection: IDbConnection) (task: Task) =
        connection
        |> Db.newCommand "INSERT INTO task (name, project) VALUES (@name, @project)"
        |> Db.setParams
            [ "@name", SqlType.String task.name
              "@project", SqlType.String task.project.name ]
        |> Db.exec

    let updateProjectTask (connection: IDbConnection) (task: Task) (newTime: int) =
        connection
        |> Db.newCommand "UPDATE task SET time = @newTime WHERE project = @project AND name = @name"
        |> Db.setParams
            [ "@newTime", SqlType.Int newTime
              "@project", SqlType.String task.project.name
              "@name", SqlType.String task.name ]
        |> Db.exec

let isValidChoice (choices: 'a array) (input: string) =
    match Int32.TryParse input with
    | (true, index) when 0 <= index - 1 && index - 1 < choices.Length -> Some choices.[index - 1]
    | (true, _) -> None
    | (false, _) -> None

let isValidName (s: string) =
    match String.IsNullOrWhiteSpace s with
    | true -> None
    | false -> Some s

let createProject name =
    let project = { name = name }
    Database.createProject connection project
    project

let createProjectAndNotify name =
    printfn $"Creating a new project \"{name}\""
    createProject name

let createTask project name =
    let task =
        { project = project
          name = name
          time = 0 }

    Database.createProjectTask connection task
    task

let createTaskAndNotify (project: Project) name =
    printfn $"Creating a new task \"{name}\" for \"{project.name}\""
    createTask project name

let updateTask task newTime =
    Database.updateProjectTask connection task newTime

module Tracker =
    let private showWorkingTime (project: Project) task diff sum =
        printf $"Working on {project.name}/{task.name} for {diff} minutes ({sum} minutes total)\r"

    let private setTimer (project: Project) (task: Task) (start: DateTime) (timer: Timer) =
        let callback (e: ElapsedEventArgs) =
            let diff = Math.Round((e.SignalTime - start).TotalMinutes) |> int
            let sum = diff + task.time
            showWorkingTime project task diff sum
            ()

        timer.AutoReset <- true
        timer.Elapsed.Add(callback)
        timer.Enabled <- true
        ()

    let track (project: Project) (task: Task) (timer: Timer) =
        let start = DateTime.Now
        printfn ""
        printfn $"Press any key to stop."
        showWorkingTime project task 0 task.time
        setTimer project task start timer
        Console.ReadLine() |> ignore
        timer.Stop()
        let diff = (DateTime.Now - start).TotalMinutes |> Math.Round |> int
        let sum = diff + task.time

        printfn $"Storing progress of {diff} minutes for a total of {sum} minutes on {project.name}/{task.name}..."
        updateTask task sum

open Tracker

let enterNewTask project timer =
    printf "Enter a new task: "
    let taskName = Console.ReadLine()

    match isValidName taskName with
    | Some name ->
        let task = createTaskAndNotify project name
        track project task timer
    | None -> printfn "No valid task name provided, exit."


[<EntryPoint>]
let main args =
    let timer = new Timer(60000)
    Database.createTables connection
    let projects = Database.getProjects connection

    match projects with
    | [||] ->
        printfn "No projects yet."
        printf "Start a new project by giving it a name: "
    | _ ->
        projects |> Array.iteri (fun i p -> printfn $"[{i + 1}] {p.name}")
        printf "Enter project number or the name of a new project: "

    let projectChoice = Console.ReadLine()

    match isValidChoice projects projectChoice with
    | Some project ->
        let projectTasks = Database.getProjectTasks connection project

        match projectTasks with
        | [||] ->
            printfn "This project has no tasks yet."
            enterNewTask project timer
        | _ ->
            printfn $"These tasks are assigned to \"{project.name}\""

            projectTasks
            |> Array.iteri (fun i t -> printfn $"[{i + 1}] {t.name} / Spent: {t.time}")

            printf "Enter task number to continue tracking or a new name to start a new task: "
            let taskChoice = Console.ReadLine()

            match isValidChoice projectTasks taskChoice with
            | Some task -> track project task timer
            | None ->
                match isValidName taskChoice with
                | Some name ->
                    let task = createTaskAndNotify project name
                    track project task timer
                | None -> printfn "No valid task name provided, exit."
    | None ->
        match isValidName projectChoice with
        | Some name ->
            let project = createProjectAndNotify name
            enterNewTask project timer
        | None -> printfn "No valid project name provided, exit."

    0
