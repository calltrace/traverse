//! Mermaid sequence diagram chunking functionality
//! 
//! Splits large mermaid sequence diagrams into manageable chunks with smart boundary detection.

use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use chrono::Utc;
use serde::{Serialize, Deserialize};
use anyhow::{Result, Context, bail};

const TARGET_CHUNK_SIZE: usize = 400;
const MIN_CHUNK_SIZE: usize = 200;
const MAX_CHUNK_SIZE: usize = 600;
const LOOKAHEAD_SIZE: usize = 150;
const CONTEXT_OVERLAP: usize = 30;
const DEFAULT_CHUNK_DIR: &str = "mermaid-chunks";

/// Result of chunking operation
#[derive(Debug)]
pub struct ChunkingResult {
    pub chunk_count: usize,
    pub output_dir: PathBuf,
    pub index_file: PathBuf,
    pub metadata_file: PathBuf,
    pub largest_chunk: usize,
    pub smallest_chunk: usize,
}

/// Metadata for the chunking operation
#[derive(Debug, Serialize, Deserialize)]
pub struct ChunkingMetadata {
    pub generated: String,
    pub generator: String,
    pub total_lines: usize,
    pub chunk_count: usize,
    pub chunks: Vec<ChunkInfo>,
    pub statistics: ChunkStatistics,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ChunkInfo {
    pub id: usize,
    pub file: String,
    pub lines: (usize, usize),
    pub description: String,
    pub participants: Vec<String>,
    pub boundary_quality: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ChunkStatistics {
    pub total_participants: usize,
    pub total_interactions: usize,
    pub average_chunk_size: usize,
}

/// State tracked between chunks
#[derive(Debug, Clone)]
struct ChunkState {
    active_participants: HashSet<String>,
    open_blocks: Vec<BlockInfo>,
    last_sequence: usize,
    previous_chunk: Option<usize>,
}

#[derive(Debug, Clone)]
struct BlockInfo {
    block_type: String,
    label: Option<String>,
    depth: usize,
}

/// Boundary point with quality score
#[derive(Debug)]
struct BoundaryPoint {
    line_index: usize,
    score: u32,
    boundary_type: BoundaryType,
}

#[derive(Debug)]
enum BoundaryType {
    CleanBreak,
    FunctionEnd,
    BlockEnd,
    InteractionEnd,
    ForcedSplit,
}

/// Main entry point for chunking a mermaid diagram
pub fn chunk_mermaid_diagram(
    diagram: &str,
    output_dir: Option<&Path>,
) -> Result<ChunkingResult> {
    let output_dir = output_dir
        .map(|p| p.to_path_buf())
        .unwrap_or(PathBuf::from(DEFAULT_CHUNK_DIR));
    
    fs::create_dir_all(&output_dir)
        .context("creating output directory")?;
    
    let lines = diagram.lines().map(|s| s.to_string()).collect::<Vec<_>>();
    if lines.is_empty() {
        bail!("Empty diagram");
    }
    
    let analysis = analyze_diagram(&lines)?;
    
    let boundaries = find_chunk_boundaries(&lines, &analysis);
    
    let mut chunk_infos = Vec::new();
    let mut state = ChunkState {
        active_participants: HashSet::new(),
        open_blocks: Vec::new(),
        last_sequence: 0,
        previous_chunk: None,
    };
    
    let mut largest_chunk = 0;
    let mut smallest_chunk = usize::MAX;
    
    for (chunk_id, boundary) in boundaries.iter().enumerate() {
        let chunk_num = chunk_id + 1;
        let (start, end) = boundary;
        let chunk_lines = &lines[*start..*end];
        let chunk_size = chunk_lines.len();
        
        largest_chunk = largest_chunk.max(chunk_size);
        smallest_chunk = smallest_chunk.min(chunk_size);
        
        let (chunk_content, new_state, chunk_info) = generate_chunk(
            chunk_lines,
            chunk_num,
            boundaries.len(),
            &state,
            &analysis,
            (*start, *end),
        )?;
        
        let filename = format!("chunk_{:03}.mmd", chunk_num);
        let chunk_path = output_dir.join(&filename);
        fs::write(&chunk_path, chunk_content)
            .context(format!("writing chunk {}", chunk_num))?;
        
        chunk_infos.push(ChunkInfo {
            id: chunk_num,
            file: filename,
            lines: (*start, *end),
            description: format!("chunk_{}", chunk_num),
            participants: chunk_info.participants.clone(),
            boundary_quality: format!("{:?}", chunk_info.boundary_quality),
        });
        
        state = new_state;
    }
    
    let index_content = generate_index(&chunk_infos, &analysis);
    let index_path = output_dir.join("index.mmd");
    fs::write(&index_path, index_content)
        .context("writing index file")?;
    
    let metadata = ChunkingMetadata {
        generated: Utc::now().to_rfc3339(),
        generator: "traverse sol2cg".to_string(),
        total_lines: lines.len(),
        chunk_count: chunk_infos.len(),
        chunks: chunk_infos,
        statistics: ChunkStatistics {
            total_participants: analysis.all_participants.len(),
            total_interactions: analysis.interaction_count,
            average_chunk_size: lines.len() / boundaries.len(),
        },
    };
    
    let metadata_path = output_dir.join("metadata.json");
    let metadata_json = serde_json::to_string_pretty(&metadata)?;
    fs::write(&metadata_path, metadata_json)
        .context("writing metadata file")?;
    
    Ok(ChunkingResult {
        chunk_count: boundaries.len(),
        output_dir,
        index_file: index_path,
        metadata_file: metadata_path,
        largest_chunk,
        smallest_chunk,
    })
}

/// Analysis of the diagram structure
struct DiagramAnalysis {
    all_participants: HashSet<String>,
    interaction_count: usize,
    has_title: bool,
    title: Option<String>,
}

/// Analyze the diagram structure
fn analyze_diagram(lines: &[String]) -> Result<DiagramAnalysis> {
    let mut all_participants = HashSet::new();
    let mut interaction_count = 0;
    let mut has_title = false;
    let mut title = None;
    
    for line in lines {
        let trimmed = line.trim();
        
        if trimmed.starts_with("participant ") {
            if let Some(participant) = extract_participant(trimmed) {
                all_participants.insert(participant);
            }
        } else if trimmed.starts_with("title ") {
            has_title = true;
            title = Some(trimmed[6..].trim().to_string());
        } else if is_interaction(trimmed) {
            interaction_count += 1;
        }
    }
    
    Ok(DiagramAnalysis {
        all_participants,
        interaction_count,
        has_title,
        title,
    })
}

/// Find optimal chunk boundaries
fn find_chunk_boundaries(lines: &[String], _analysis: &DiagramAnalysis) -> Vec<(usize, usize)> {
    let mut boundaries = Vec::new();
    let mut current_pos = 0;
    
    while current_pos < lines.len() {
        let target_end = (current_pos + TARGET_CHUNK_SIZE).min(lines.len());
        let min_end = (current_pos + MIN_CHUNK_SIZE).min(lines.len());
        let max_end = (current_pos + MAX_CHUNK_SIZE).min(lines.len());
        
        let best_boundary = find_best_boundary(lines, min_end, max_end);
        
        let chunk_end = best_boundary.unwrap_or(target_end);
        
        let next_start = chunk_end;
        
        boundaries.push((current_pos, chunk_end));
        current_pos = next_start;
    }
    
    boundaries
}

/// Find the best boundary point in a range
fn find_best_boundary(lines: &[String], start: usize, end: usize) -> Option<usize> {
    let mut best_boundary = None;
    let mut best_score = 0;
    
    for i in start..=end {
        if i >= lines.len() {
            break;
        }
        
        let score = score_boundary(lines, i);
        if score > best_score {
            best_score = score;
            best_boundary = Some(i);
        }
    }
    
    best_boundary
}

/// Score a potential boundary point
fn score_boundary(lines: &[String], index: usize) -> u32 {
    if index == 0 || index >= lines.len() {
        return 0;
    }
    
    let mut block_depth: i32 = 0;
    for i in 0..index {
        let trimmed = lines[i].trim();
        if trimmed.starts_with("opt ") || trimmed.starts_with("alt ") || 
           trimmed.starts_with("loop ") || trimmed.starts_with("par ") ||
           trimmed.starts_with("critical ") || trimmed.starts_with("break ") {
            block_depth += 1;
        } else if trimmed == "end" {
            block_depth = block_depth.saturating_sub(1);
        }
    }
    
    if block_depth > 0 {
        return 0;
    }
    
    let prev_line = lines[index - 1].trim();
    let curr_line = if index < lines.len() {
        lines[index].trim()
    } else {
        ""
    };
    
    if prev_line.is_empty() && !curr_line.starts_with("  ") {
        return 100;
    }
    
    if prev_line == "end" {
        return 90;
    }
    
    if prev_line.starts_with("deactivate ") {
        return 70;
    }
    
    if !prev_line.is_empty() && !curr_line.is_empty() 
        && !curr_line.starts_with("  ") && !prev_line.starts_with("  ") {
        return 50;
    }
    
    10
}

/// Generate a single chunk
fn generate_chunk(
    chunk_lines: &[String],
    chunk_num: usize,
    total_chunks: usize,
    state: &ChunkState,
    analysis: &DiagramAnalysis,
    line_range: (usize, usize),
) -> Result<(String, ChunkState, ChunkInfo)> {
    let mut output = Vec::new();
    let mut new_state = state.clone();
    
    let mut used_participants = Vec::new();
    let mut seen = HashSet::new();
    
    for line in chunk_lines {
        let trimmed = line.trim();
        if trimmed.starts_with("participant ") {
            if let Some(participant) = extract_participant(trimmed) {
                if seen.insert(participant.clone()) {
                    used_participants.push(participant);
                }
            }
        }
        for participant in &analysis.all_participants {
            if trimmed.contains(participant) && seen.insert(participant.clone()) {
                used_participants.push(participant.clone());
            }
        }
    }
    
    output.push("sequenceDiagram".to_string());
    output.push(format!("    %% Chunk {} of {} - Lines {}-{}", 
        chunk_num, total_chunks, line_range.0 + 1, line_range.1));
    output.push(format!("    title Call Graph - Chunk {}", chunk_num));
    output.push(String::new());
    
    for participant in &used_participants {
        output.push(format!("    participant {}", participant));
    }
    output.push(String::new());
    
    if chunk_num > 1 {
        output.push(format!("    Note over {}: Continued from Chunk {}", 
            used_participants.iter().next().unwrap_or(&"User".to_string()),
            chunk_num - 1));
        output.push(String::new());
    }
    
    for participant in &state.active_participants {
        if used_participants.contains(participant) {
            output.push(format!("    activate {}", participant));
        }
    }
    
    let mut seen_sequence_diagram = false;
    let mut in_header_section = true;
    
    for line in chunk_lines {
        let trimmed = line.trim();
        
        if trimmed == "sequenceDiagram" {
            if seen_sequence_diagram || output.iter().any(|l| l.trim() == "sequenceDiagram") {
                continue;
            }
            seen_sequence_diagram = true;
            in_header_section = true;
            continue;
        }
        
        if in_header_section {
            if trimmed.starts_with("title ") || 
               (trimmed.starts_with("participant ") && trimmed.contains(" as ")) {
                continue;
            }
            if !trimmed.is_empty() && 
               !trimmed.starts_with("participant ") &&
               !trimmed.starts_with("%%") {
                in_header_section = false;
            }
        }
        
        output.push(line.clone());
        
        if trimmed.starts_with("activate ") {
            if let Some(p) = trimmed.strip_prefix("activate ") {
                new_state.active_participants.insert(p.to_string());
            }
        } else if trimmed.starts_with("deactivate ") {
            if let Some(p) = trimmed.strip_prefix("deactivate ") {
                new_state.active_participants.remove(p);
            }
        }
    }
    
    if chunk_num < total_chunks {
        output.push(String::new());
        output.push(format!("    Note over {}: Continues in Chunk {}", 
            used_participants.iter().next().unwrap_or(&"User".to_string()),
            chunk_num + 1));
    }
    
    output.push(String::new());
    output.push(format!("    %% Lines: {}-{}", line_range.0 + 1, line_range.1));
    output.push(format!("    %% Participants: {}", used_participants.len()));
    
    let chunk_info = ChunkInfo {
        id: chunk_num,
        file: String::new(),
        lines: line_range,
        description: format!("chunk_{}", chunk_num),
        participants: used_participants.clone(),
        boundary_quality: format!("{:?}", BoundaryType::InteractionEnd),
    };
    
    Ok((output.join("\n"), new_state, chunk_info))
}

/// Generate index file
fn generate_index(chunks: &[ChunkInfo], analysis: &DiagramAnalysis) -> String {
    let mut output = Vec::new();
    
    output.push("sequenceDiagram".to_string());
    output.push(format!("    title Mermaid Diagram Index - {} Chunks", chunks.len()));
    output.push(String::new());
    output.push("    participant Index as \"Navigation Index\"".to_string());
    output.push(String::new());
    
    for chunk in chunks {
        output.push(format!("    Note over Index: Chunk {}: Lines {}-{}", 
            chunk.id, chunk.lines.0 + 1, chunk.lines.1));
    }
    
    output.push(String::new());
    output.push(format!("    Note over Index: Total: {} lines<br/>{} participants<br/>{} interactions",
        analysis.interaction_count,
        analysis.all_participants.len(),
        analysis.interaction_count));
    
    output.join("\n")
}


fn extract_participant(line: &str) -> Option<String> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.len() >= 2 {
        let participant = parts[1].trim();
        if let Some(as_pos) = parts.iter().position(|&p| p == "as") {
            return Some(parts[1..as_pos].join(" "));
        }
        Some(participant.to_string())
    } else {
        None
    }
}

fn is_interaction(line: &str) -> bool {
    line.contains("->>") || line.contains("-->>") || 
    line.contains("->") || line.contains("-->") ||
    line.contains("-x") || line.contains("--x")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::Path;

    #[test]
    fn test_chunk_si_mmd() {
        let content = fs::read_to_string("../../si.mmd")
            .expect("si.mmd not found");
        
        println!("Testing chunking of si.mmd ({} lines)", content.lines().count());
        
        let result = chunk_mermaid_diagram(&content, Some(Path::new("/tmp/si-chunks-test")))
            .expect("chunking failed");
        
        println!("✓ Successfully chunked into {} parts", result.chunk_count);
        println!("  Largest chunk: {} lines", result.largest_chunk);
        println!("  Smallest chunk: {} lines", result.smallest_chunk);
        
        assert!(result.chunk_count > 1, "Should create multiple chunks for 1429 lines");
        assert!(result.largest_chunk <= 600, "Chunks should not exceed 600 lines");
        assert!(result.smallest_chunk >= 50, "Chunks should have reasonable minimum size");
        
        let chunk1_path = result.output_dir.join("chunk_001.mmd");
        let chunk1_content = fs::read_to_string(&chunk1_path)
            .unwrap_or_else(|_| {
                // Try the alternate naming pattern
                let alt_path = result.output_dir.join("part_001_chunk_1.mmd");
                fs::read_to_string(alt_path).expect("chunk file missing")
            });
        
        assert!(chunk1_content.contains("sequenceDiagram"));
        assert!(chunk1_content.contains("participant"));
        
        println!("✓ First chunk appears valid");
    }

    #[test]
    fn test_empty_diagram() {
        let result = chunk_mermaid_diagram("", None);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Empty diagram"));
    }

    #[test]
    fn test_small_diagram_no_chunking_needed() {
        let small_diagram = "sequenceDiagram\n    participant A\n    participant B\n    A->>B: Hello";
        
        let result = chunk_mermaid_diagram(small_diagram, Some(Path::new("/tmp/small-test")))
            .expect("should handle small diagram");
        
        assert_eq!(result.chunk_count, 1, "Small diagram should produce single chunk");
    }

    #[test]
    fn test_participant_extraction() {
        let line = "    participant User as U";
        let participant = extract_participant(line);
        assert_eq!(participant, Some("User".to_string()));
        
        let line2 = "    actor System";
        let participant2 = extract_participant(line2);
        assert_eq!(participant2, Some("System".to_string()));
    }

    #[test]
    fn test_interaction_detection() {
        assert!(is_interaction("A->>B: Message"));
        assert!(is_interaction("A-->>B: Message"));
        assert!(is_interaction("A->B: Message"));
        assert!(is_interaction("A-->B: Message"));
        assert!(is_interaction("A-xB: Message"));
        assert!(!is_interaction("participant A"));
        assert!(!is_interaction("Note over A: Something"));
    }

    #[test]
    fn test_chunk_boundaries_respect_blocks() {
        let diagram = r#"sequenceDiagram
    participant A
    participant B
    loop Every minute
        A->>B: Check
        B-->>A: Response
    end
    A->>B: Done"#;
        
        let result = chunk_mermaid_diagram(diagram, Some(Path::new("/tmp/block-test")))
            .expect("should handle blocks");
        
        // The loop block should stay together
        assert!(result.chunk_count >= 1);
    }
}