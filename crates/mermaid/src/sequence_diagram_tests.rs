//
// Test suite based on the Mermaid official one: https://github.com/mermaid-js/mermaid/blob/master/packages/mermaid/src/diagrams/sequence/sequenceDiagram.spec.js
//
use crate::sequence_diagram_parser::{Rule, SequenceDiagramParser};
use pest::Parser;

// Helper function to test if a diagram parses successfully
fn test_parse(input: &str) -> bool {
    match SequenceDiagramParser::parse(Rule::sequence_diagram, input) {
        Ok(_) => true,
        Err(e) => {
            println!("Parse error: {}", e);
            false
        }
    }
}

#[test]
fn test_basic_sequence_diagram() {
    let diagram = r#"sequenceDiagram
    Alice->Bob:Hello Bob, how are you?
    Note right of Bob: Bob thinks
    Bob-->Alice: I am good thanks!"#;

    assert!(test_parse(diagram));
}

#[test]
fn test_multiple_sequence_diagrams() {
    // Test that multiple diagrams don't interfere with each other
    let diagram1 = r#"sequenceDiagram
    Alice->Bob:Hello Bob, how are you?
    Bob-->Alice: I am good thanks!"#;

    let diagram2 = r#"sequenceDiagram
    Alice->Bob:Hello Bob, how are you?
    Bob-->Alice: I am good thanks!"#;

    assert!(test_parse(diagram1));
    assert!(test_parse(diagram2));

    let diagram3 = r#"sequenceDiagram
    Alice->John:Hello John, how are you?
    John-->Alice: I am good thanks!"#;

    assert!(test_parse(diagram3));
}

#[test]
fn test_diagram_with_title() {
    let diagram_with_title = r#"sequenceDiagram
    title: Diagram Title
    Alice->Bob:Hello Bob, how are you?
    Note right of Bob: Bob thinks
    Bob-->Alice: I am good thanks!"#;

    assert!(test_parse(diagram_with_title));

    let diagram_with_title_no_colon = r#"sequenceDiagram
    title Diagram Title
    Alice->Bob:Hello Bob, how are you?
    Note right of Bob: Bob thinks
    Bob-->Alice: I am good thanks!"#;

    assert!(test_parse(diagram_with_title_no_colon));
}

#[test]
fn test_diagram_with_accessibility() {
    let diagram_with_acc = r#"sequenceDiagram
    title: Diagram Title
    accTitle: This is the title
    accDescr: Accessibility Description
    Alice->Bob:Hello Bob, how are you?"#;

    assert!(test_parse(diagram_with_acc));

    let diagram_with_multiline_acc = r#"sequenceDiagram
    accTitle: This is the title
    accDescr {
    Accessibility
    Description
    }
    Alice->Bob:Hello Bob, how are you?"#;

    assert!(test_parse(diagram_with_multiline_acc));
}

#[test]
fn test_actor_names_with_spaces_and_dashes() {
    let diagram_with_spaces = r#"sequenceDiagram
    Alice->Bob:Hello Bob, how are - you?
    Bob-->Alice: I am good thanks!"#;

    assert!(test_parse(diagram_with_spaces));

    let diagram_with_dashes = r#"sequenceDiagram
    Alice-in-Wonderland->Bob:Hello Bob, how are - you?
    Bob-->Alice-in-Wonderland:I am good thanks!"#;

    assert!(test_parse(diagram_with_dashes));
}

#[test]
fn test_participant_with_dashes() {
    let diagram = r#"sequenceDiagram
    participant Alice-in-Wonderland
    participant Bob
    Alice-in-Wonderland->Bob:Hello Bob, how are - you?
    Bob-->Alice-in-Wonderland:I am good thanks!"#;

    assert!(test_parse(diagram));
}

#[test]
fn test_alias_participants() {
    let diagram = r#"sequenceDiagram
    participant A as Alice
    participant B as Bob
    A->B:Hello Bob, how are you?
    B-->A: I am good thanks!"#;

    assert!(test_parse(diagram));
}

#[test]
fn test_alias_actors() {
    let diagram = r#"sequenceDiagram
    actor A as Alice
    actor B as Bob
    A->B:Hello Bob, how are you?
    B-->A: I am good thanks!"#;

    assert!(test_parse(diagram));
}

#[test]
fn test_mixed_actors_and_participants() {
    let diagram = r#"sequenceDiagram
    actor Alice as Alice2
    actor Bob
    participant John as John2
    participant Mandy
    Alice->>Bob: Hi Bob
    Bob->>Alice: Hi Alice
    Alice->>John: Hi John
    John->>Mandy: Hi Mandy
    Mandy ->>Joan: Hi Joan"#;

    assert!(test_parse(diagram));
}

#[test]
fn test_arrow_types() {
    // Test solid arrow
    let solid_arrow = r#"sequenceDiagram
    Alice->>Bob:Hello Bob, how are you?"#;
    assert!(test_parse(solid_arrow));

    // Test dotted arrow
    let dotted_arrow = r#"sequenceDiagram
    Alice-->>Bob:Hello Bob, how are you?"#;
    assert!(test_parse(dotted_arrow));

    // Test solid cross
    let solid_cross = r#"sequenceDiagram
    Alice-xBob:Hello Bob, how are you?"#;
    assert!(test_parse(solid_cross));

    // Test dotted cross
    let dotted_cross = r#"sequenceDiagram
    Alice--xBob:Hello Bob, how are you?"#;
    assert!(test_parse(dotted_cross));

    // Test solid point
    let solid_point = r#"sequenceDiagram
    Alice-)Bob:Hello Bob, how are you?"#;
    assert!(test_parse(solid_point));

    // Test dotted point
    let dotted_point = r#"sequenceDiagram
    Alice--)Bob:Hello Bob, how are you?"#;
    assert!(test_parse(dotted_point));

    // Test bidirectional solid arrow
    let bidirectional_solid = r#"sequenceDiagram
    Alice<<->>Bob:Hello Bob, how are you?"#;
    assert!(test_parse(bidirectional_solid));

    // Test bidirectional dotted arrow
    let bidirectional_dotted = r#"sequenceDiagram
    Alice<<-->>Bob:Hello Bob, how are you?"#;
    assert!(test_parse(bidirectional_dotted));
}

#[test]
fn test_activation_deactivation() {
    let diagram = r#"sequenceDiagram
    Alice-->>Bob:Hello Bob, how are you?
    activate Bob
    Bob-->>Alice:Hello Alice, I'm fine and you?
    deactivate Bob"#;

    assert!(test_parse(diagram));

    // Test one-line notation
    let one_line_notation = r#"sequenceDiagram
    Alice-->>+Bob:Hello Bob, how are you?
    Bob-->>-Alice:Hello Alice, I'm fine and you?"#;

    assert!(test_parse(one_line_notation));

    // Test stacked activations
    let stacked_activations = r#"sequenceDiagram
    Alice-->>+Bob:Hello Bob, how are you?
    Bob-->>+Carol:Carol, let me introduce Alice?
    Bob-->>-Alice:Hello Alice, please meet Carol?
    Carol->>-Bob:Oh Bob, I'm so happy to be here!"#;

    assert!(test_parse(stacked_activations));
}

#[test]
fn test_comments() {
    let diagram_with_comments = r#"sequenceDiagram
    Alice->Bob: Hello Bob, how are you?
    %% Comment
    Note right of Bob: Bob thinks
    Bob-->Alice: I am good thanks!"#;

    assert!(test_parse(diagram_with_comments));

    let diagram_with_newlines = r#"sequenceDiagram
    Alice->Bob: Hello Bob, how are you?

    %% Comment
    Note right of Bob: Bob thinks
    Bob-->Alice: I am good thanks!"#;

    assert!(test_parse(diagram_with_newlines));
}

#[test]
fn test_semicolons() {
    let diagram_with_semicolons = r#"sequenceDiagram;Alice->Bob: Hello Bob, how are you?;Note right of Bob: Bob thinks;Bob-->Alice: I am good thanks!;"#;

    assert!(test_parse(diagram_with_semicolons));
}

#[test]
fn test_leading_spaces() {
    let diagram_with_one_space = r#"sequenceDiagram
 Alice->Bob: Hello Bob, how are you?

%% Comment
Note right of Bob: Bob thinks
Bob-->Alice: I am good thanks!"#;

    assert!(test_parse(diagram_with_one_space));

    let diagram_with_several_spaces = r#"sequenceDiagram
   Alice->Bob: Hello Bob, how are you?

%% Comment
Note right of Bob: Bob thinks
Bob-->Alice: I am good thanks!"#;

    assert!(test_parse(diagram_with_several_spaces));

    let diagram_with_mixed_spaces = r#"sequenceDiagram
participant Alice
participant Bob
Alice->John: Hello John, how are you?
    loop Healthcheck
John->John: Fight against hypochondria
 end
Note right of John: Rational thoughts<br/>prevail...
    John-->Alice: Great!
    John->Bob: How about you?
Bob-->John: Jolly good!"#;

    assert!(test_parse(diagram_with_mixed_spaces));
}

#[test]
fn test_line_breaks() {
    let diagram_with_breaks = r#"sequenceDiagram
participant 1 as multiline<br>text
participant 2 as multiline<br/>text
participant 3 as multiline<br />text
participant 4 as multiline<br 	/>text
1->>2: multiline<br>text
note right of 2: multiline<br>text
2->>3: multiline<br/>text
note right of 3: multiline<br/>text
3->>4: multiline<br />text
note right of 4: multiline<br />text
4->>1: multiline<br 	/>text
note right of 1: multiline<br 	/>text"#;

    assert!(test_parse(diagram_with_breaks));
}

#[test]
fn test_wrap_nowrap() {
    let diagram_without_wrap = r#"sequenceDiagram
participant 1
participant 2
participant 3
participant 4
1->>2: single-line text
note right of 2: single-line text
2->>3:nowrap: single-line text
note right of 3:nowrap: single-line text
3->>4: multiline<br/>text
note right of 4: multiline<br/>text
4->>1:nowrap: multiline<br/>text
note right of 1:nowrap: multiline<br/>text"#;

    assert!(test_parse(diagram_without_wrap));

    let diagram_with_wrap = r#"sequenceDiagram
participant 1
participant 2
participant 3
participant 4
1->>2:wrap: single-line text
note right of 2:wrap: single-line text
2->>3:wrap: multiline<br/>text
note right of 3:wrap: multiline<br/>text"#;

    assert!(test_parse(diagram_with_wrap));
}

#[test]
fn test_notes() {
    // Test note over single actor
    let note_over_single = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?
Note over Bob: Bob thinks"#;

    assert!(test_parse(note_over_single));

    // Test note over multiple actors
    let note_over_multiple = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?
Note over Alice,Bob: confusion
Note over Bob,Alice: resolution"#;

    assert!(test_parse(note_over_multiple));
}

#[test]
fn test_loops() {
    let diagram_with_loop = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?

%% Comment
Note right of Bob: Bob thinks
loop Multiple happy responses

Bob-->Alice: I am good thanks!
end"#;

    assert!(test_parse(diagram_with_loop));
}

#[test]
fn test_rect() {
    let diagram_with_rect = r#"sequenceDiagram
    Alice->Bob: Hello Bob, how are you?
    %% Comment
    rect rgb(200, 255, 200)
    Note right of Bob: Bob thinks
    Bob-->Alice: I am good thanks
    end"#;

    assert!(test_parse(diagram_with_rect));

    // Test nested rects
    let nested_rects = r#"sequenceDiagram
    Alice->Bob: Hello Bob, how are you?
    %% Comment
    rect rgb(200, 255, 200)
    rect rgb(0, 0, 0)
    Note right of Bob: Bob thinks
    end
    Bob-->Alice: I am good thanks
    end"#;

    assert!(test_parse(nested_rects));
}

#[test]
fn test_opt() {
    let diagram_with_opt = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?

%% Comment
Note right of Bob: Bob thinks
opt Perhaps a happy response

Bob-->Alice: I am good thanks!
end"#;

    assert!(test_parse(diagram_with_opt));
}

#[test]
fn test_alt() {
    let diagram_with_alt = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?

%% Comment
Note right of Bob: Bob thinks
alt isWell

Bob-->Alice: I am good thanks!
else isSick
Bob-->Alice: Feel sick...
end"#;

    assert!(test_parse(diagram_with_alt));

    // Test alt with multiple elses
    let alt_with_multiple_elses = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?

%% Comment
Note right of Bob: Bob thinks
alt isWell

Bob-->Alice: I am good thanks!
else isSick
Bob-->Alice: Feel sick...
else default
Bob-->Alice: :-)
end"#;

    assert!(test_parse(alt_with_multiple_elses));
}

#[test]
fn test_critical() {
    // Test critical without options
    let critical_without_options = r#"sequenceDiagram
    critical Establish a connection to the DB
        Service->DB: connect
    end"#;

    assert!(test_parse(critical_without_options));

    // Test critical with options
    let critical_with_options = r#"sequenceDiagram
    critical Establish a connection to the DB
        Service->DB: connect
    opt Network timeout
        Service->Service: Log error
    end
    opt Credentials rejected
        Service->Service: Log different error
    end
    end"#;

    assert!(test_parse(critical_with_options));
}

#[test]
fn test_break() {
    let diagram_with_break = r#"sequenceDiagram
    Consumer-->API: Book something
    API-->BookingService: Start booking process
    break when the booking process fails
        API-->Consumer: show failure
    end
    API-->BillingService: Start billing process"#;

    assert!(test_parse(diagram_with_break));
}

#[test]
fn test_par() {
    let diagram_with_par = r#"sequenceDiagram
par Parallel one
Alice->>Bob: Hello Bob, how are you?
Bob-->>Alice: I am good thanks!
and Parallel two
Alice->>Bob: Are you OK?
Bob-->>Alice: Fine!
and Parallel three
Alice->>Bob: What do you think about it?
Bob-->>Alice: It's good!
end"#;

    assert!(test_parse(diagram_with_par));
}

#[test]
fn test_par_over() {
    let diagram_with_par_over = r#"sequenceDiagram
par_over Parallel overlap
Alice ->> Bob: Message
Note left of Alice: Alice note
Note right of Bob: Bob note
end"#;

    assert!(test_parse(diagram_with_par_over));
}

#[test]
fn test_special_characters() {
    // Test special characters in signals
    let special_chars_in_signals = r#"sequenceDiagram
Alice->Bob: -:<>;# comment"#;

    assert!(test_parse(special_chars_in_signals));

    // Test special characters in notes
    let special_chars_in_notes = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?
Note right of Bob: -:<>,;# comment"#;

    assert!(test_parse(special_chars_in_notes));

    // Test special characters in loop
    let special_chars_in_loop = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?
loop -:<>,;# comment
Bob-->Alice: I am good thanks!
end"#;

    assert!(test_parse(special_chars_in_loop));

    // Test special characters in opt
    let special_chars_in_opt = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?
opt -:<>,;# comment
Bob-->Alice: I am good thanks!
end"#;

    assert!(test_parse(special_chars_in_opt));

    // Test special characters in alt
    let special_chars_in_alt = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?
alt -:<>,;# comment
Bob-->Alice: I am good thanks!
else ,<>:-#; comment
Bob-->Alice: I am good thanks!
end"#;

    assert!(test_parse(special_chars_in_alt));

    // Test special characters in par
    let special_chars_in_par = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?
par -:<>,;# comment
Bob-->Alice: I am good thanks!
and ,<>:-#; comment
Bob-->Alice: I am good thanks!
end"#;

    assert!(test_parse(special_chars_in_par));
}

#[test]
fn test_no_label_statements() {
    // Test no-label loop
    let no_label_loop = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?
loop
Bob-->Alice: I am good thanks!
end"#;

    assert!(test_parse(no_label_loop));

    // Test no-label opt
    let no_label_opt = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?
opt # comment
Bob-->Alice: I am good thanks!
end"#;

    assert!(test_parse(no_label_opt));

    // Test no-label alt
    let no_label_alt = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?
alt;Bob-->Alice: I am good thanks!
else # comment
Bob-->Alice: I am good thanks!
end"#;

    assert!(test_parse(no_label_alt));

    // Test no-label par
    let no_label_par = r#"sequenceDiagram
Alice->Bob: Hello Bob, how are you?
par;Bob-->Alice: I am good thanks!
and # comment
Bob-->Alice: I am good thanks!
end"#;

    assert!(test_parse(no_label_par));
}

#[test]
fn test_links() {
    let diagram_with_links = r#"sequenceDiagram
participant a as Alice
participant b as Bob
participant c as Charlie
links a: { "Repo": "https://repo.contoso.com/", "Dashboard": "https://dashboard.contoso.com/" }
links b: { "Dashboard": "https://dashboard.contoso.com/" }
links a: { "On-Call": "https://oncall.contoso.com/?svc=alice" }
link a: Endpoint @ https://alice.contoso.com
link a: Swagger @ https://swagger.contoso.com
link a: Tests @ https://tests.contoso.com/?svc=alice@contoso.com"#;

    assert!(test_parse(diagram_with_links));
}

#[test]
fn test_properties() {
    let diagram_with_properties = r#"sequenceDiagram
participant a as Alice
participant b as Bob
participant c as Charlie
properties a: {"class": "internal-service-actor", "icon": "@clock"}
properties b: {"class": "external-service-actor", "icon": "@computer"}"#;

    assert!(test_parse(diagram_with_properties));
}

#[test]
fn test_box() {
    // Test box with color
    let box_with_color = r#"sequenceDiagram
box green Group 1
participant a as Alice
participant b as Bob
end
participant c as Charlie
links a: { "Repo": "https://repo.contoso.com/", "Dashboard": "https://dashboard.contoso.com/" }
links b: { "Dashboard": "https://dashboard.contoso.com/" }
links a: { "On-Call": "https://oncall.contoso.com/?svc=alice" }
link a: Endpoint @ https://alice.contoso.com
link a: Swagger @ https://swagger.contoso.com
link a: Tests @ https://tests.contoso.com/?svc=alice@contoso.com"#;

    assert!(test_parse(box_with_color));

    // Test box without color
    let box_without_color = r#"sequenceDiagram
box Group 1
participant a as Alice
participant b as Bob
end
participant c as Charlie"#;

    assert!(test_parse(box_without_color));

    // Test box without description
    let box_without_description = r#"sequenceDiagram
box Aqua
participant a as Alice
participant b as Bob
end
participant c as Charlie"#;

    assert!(test_parse(box_without_description));
}

#[test]
fn test_actor_creation_destruction() {
    // Test actor creation
    let actor_creation = r#"sequenceDiagram
participant a as Alice
a ->>b: Hello Bob?
create participant c
b-->>c: Hello c!
c ->> b: Hello b?
create actor d as Donald
a ->> d: Hello Donald?"#;

    assert!(test_parse(actor_creation));

    // Test actor destruction
    let actor_destruction = r#"sequenceDiagram
participant a as Alice
a ->>b: Hello Bob?
destroy a
b-->>a: Hello Alice!
b ->> c: Where is Alice?
destroy c
b ->> c: Where are you?"#;

    assert!(test_parse(actor_destruction));

    // Test creation and destruction of the same actor
    let creation_and_destruction = r#"sequenceDiagram
a ->>b: Hello Bob?
create participant c
b ->>c: Hello c!
c ->> b: Hello b?
destroy c
b ->> c : Bye c !"#;

    assert!(test_parse(creation_and_destruction));
}

#[test]
fn test_autonumber() {
    // Test basic autonumber
    let basic_autonumber = r#"sequenceDiagram
autonumber
Alice->Bob:Hello Bob, how are you?
Note right of Bob: Bob thinks
Bob-->Alice: I am good thanks!"#;

    assert!(test_parse(basic_autonumber));

    // Test autonumber with parameters
    let autonumber_with_params = r#"sequenceDiagram
autonumber 10
Alice->Bob:Hello Bob, how are you?
Note right of Bob: Bob thinks
Bob-->Alice: I am good thanks!"#;

    assert!(test_parse(autonumber_with_params));

    // Test autonumber with two parameters
    let autonumber_with_two_params = r#"sequenceDiagram
autonumber 10 5
Alice->Bob:Hello Bob, how are you?
Note right of Bob: Bob thinks
Bob-->Alice: I am good thanks!"#;

    assert!(test_parse(autonumber_with_two_params));

    // Test autonumber off
    let autonumber_off = r#"sequenceDiagram
autonumber
Alice->Bob:Hello Bob, how are you?
autonumber off
Note right of Bob: Bob thinks
Bob-->Alice: I am good thanks!"#;

    assert!(test_parse(autonumber_off));
}

#[test]
fn test_complex_diagram() {
    let complex_diagram = r#"sequenceDiagram
    title: Complex Sequence Diagram
    accTitle: Accessibility Title
    accDescr: This is an accessibility description
    
    participant Alice
    actor Bob
    participant Charlie as C
    
    box green System Components
    participant API
    participant DB
    end
    
    autonumber
    
    Note over Alice,Bob: Initialization
    
    Alice->>+Bob: Start process
    Bob-->>-Alice: Acknowledge
    
    par Process Data
        Alice->>API: Request data
        API->>+DB: Query
        DB-->>-API: Results
        API-->>Alice: Return data
    and Handle Authentication
        Bob->>+Charlie: Verify identity
        Charlie-->>-Bob: Identity confirmed
    end
    
    alt is authenticated
        Bob->>Alice: Grant access
        
        loop Every minute
            Alice->>API: Heartbeat
            API-->>Alice: ACK
        end
        
        opt Extra validation
            Alice->>Bob: Validate session
            Bob-->>Alice: Session valid
        end
        
    else is not authenticated
        Bob->>Alice: Deny access
        break when access denied
            Alice->>Charlie: Report failure
        end
    end
    
    critical Database transaction
        Alice->>+DB: Begin transaction
        DB-->>-Alice: Transaction started
        
        Alice->>DB: Update data
        
        opt Transaction fails
            DB-->>Alice: Error
            Alice->>DB: Rollback
        end
        
        Alice->>DB: Commit
    end
    
    Note right of Alice: Process complete
    
    create participant Logger
    Alice->>Logger: Initialize logging
    
    rect rgb(240, 240, 240)
        Alice->>Logger: Log completion
        Logger-->>Alice: Logged
    end
    
    destroy Logger
    Alice->>Logger: Shutdown logger
    
    links Alice: {"GitHub": "https://github.com/alice", "Twitter": "https://twitter.com/alice"}
    link Bob: Profile @ https://example.com/bob
    
    properties Alice: {"role": "admin", "status": "active"}
    "#;

    assert!(test_parse(complex_diagram));
}
