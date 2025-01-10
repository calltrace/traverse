use language::{Language, Solidity};


fn main() {
    // load file name into the source code variable 
    let source_code = r#"
    pragma solidity ^0.8.0; contract { } "#;
    let tree = Solidity.parse(source_code);
       

    println!("{:#?}", tree);
}
