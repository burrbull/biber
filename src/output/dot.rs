/*
use parent qw(crate::Output::base);

use crate::Config;
use crate::Constants;
use crate::Entry;
use crate::Utils;
use List::AllUtils qw( :all );
use IO::File;
use Log::Log4perl qw( :no_extra_logdie_message );
use Unicode::Normalize;
*/

/// Class for Biber output of GraphViz .dot files
pub struct Dot;

/// Initialize a crate::Output::dot object

let $graph; // accumulator for .dot string
let $graph_edges = ""; // accumulator for .dot string. Initialise as can be empty
let $state; // some state information for building output
let $in; // indentation string
let $i; // indentation level
let $gopts = crate::Config->getoption("dot_include");
let $linknode; // node to use to do cluster links

impl Dot {
  const HEAD: &str = r###"digraph Biberdata {
  compound = true;
  edge [ arrowhead=open ];
  graph [ style=filled, rankdir=LR ];
  node [
    fontsize=10,
    fillcolor=white,
    style=filled,
    shape=box ];
  "###;
  fn new(obj) -> Self {
    let $self = $class->SUPER::new($obj);

    return $self;
  }

  /// Set the output target file of a crate::Output::dot object
  /// A convenience around set_output_target so we can keep track of the
  /// filename
  fn set_output_target_file(self, dotfile) {
    $self->{output_target_file} = $dotfile;
    return IO::File->new($dotfile, '>:encoding(UTF-8)');
  }

  /// We don't use this, we output everything in one go at the end
  fn set_output_entry {
    return;
  }

  /// We don't use this, we output everything in one go at the end
  fn create_output_section {
    return;
  }

  /// Create a graph of the required things and save to .dot format
  fn output(self) {
    let $biber = $crate::MASTER;
    let $target = $self->{output_target};
    let $target_string = "Target"; // Default
    if ($self->{output_target_file}) {
      $target_string = $self->{output_target_file};
    }

    // for debugging mainly
    if !($target) {
      $target = new IO::File '>-';
    }

      debug!("Preparing final output using class {}...", __PACKAGE__);

    info!("Writing '{}' with encoding 'UTF-8'", target_string));

    out($target, Self::HEAD);

    let mut i_n = 2; // indentation
    let i = ' '.to_string(); // starting indentation

    // Loop over sections, sort so we can run tests
    // NOTE: already sorted
    for section in &biber.sections().get_sections() {
      let secnum = section.number();
      if ($gopts->{section}) {
        graph.push_str(&format!("{}subgraph \"cluster_section{secnum}\" {{\n"), i.repeat(i_n));
        i_n += 2;
        let iin = i.repeat(i_n);
        graph.push_str(&format!("{iin}label=\"Section {secnum}\";\n"));
        graph.push_str(&format!("{iin}tooltip=\"Section {secnum}\";\n"));
        graph.push_str(&format!("{iin}fontsize=\"10\";\n"));
        graph.push_str(&format!("{iin}fontname=serif;\n"));
        graph.push_str(&format!("{iin}fillcolor=\"#fce3fa\";\n"));
        graph.push_str("\n");
      }

      // First create nodes/groups for entries
      for be in section.bibentries().entries().sorted_by_key(|e| e.get_field("citekey")) {
        let citekey = be.get_field("citekey");
        $state->{$secnum}{format!("{secnum}/{citekey}")} = 1;
        let et = be.get_field("entrytype").to_uppercase();

        // colour depends on whether cited, uncited, dataonly or key alias
        let mut c = if section.has_citekey(citekey) {
          "#a0d0ff"
        } else {
          "#deefff"
        };
        if (let $options = be.get_field("options")) {
          if $options =~ m/skip(?:bib|biblist|lab)/o {
            c = "#fdffd9" ;
          }
        }
        if section.get_citekey_alias(citekey) {
          c = "#a1edec" ;
        }

        // make a set subgraph if a set member
        // This will make identically named subgraph sections for
        // every element in a set but dot is clever enough to merge them by
        // ID.
        if (let $sets = crate::Config->get_graph("set")) {
          if (let $set = $sets->{memtoset}{$citekey}) { // entry is a set member
            graph.push_str(&format!("{}subgraph \"cluster_{secnum}/set_{set}\" {{\n"), i.repeat(i_n));
            i_n += 2;
            let iin = i.repeat(i_n);
            graph.push_str(&format!("{iin}label=\"{set} (SET)\";\n"));
            graph.push_str(&format!("{iin}tooltip=\"{set} (SET)\";\n"));
            graph.push_str(&format!("{iin}fontsize=\"10\";\n"));
            graph.push_str(&format!("{iin}fontname=serif;\n"));
            graph.push_str(&format!("{iin}fillcolor=\"#e3dadc\";\n"));
            graph.push_str("\n");
          }
          if $sets->{settomem}{$citekey} { // Don't make normal nodes for sets
            continue;
          }
        }

        // Citekey aliases
        let mut aliases = String::new();
        for in alias (sort section.get_citekey_aliases()) {
          let realkey = section.get_citekey_alias(alias);
          if realkey == citekey {
            aliases.push_str(&format!("\\n{alias} (alias)"));
          }
        }

        if ($gopts->{field}) { // If granularity is at the level of fields
          graph.push_str(&format!("{}subgraph \"cluster_section{secnum}/{citekey}\" {{\n"), i.repeat(i_n));
          i_n += 2;
          let iin = i.repeat(i_n);
          graph.push_str(&format!("{iin}fontsize=\"10\";\n"));
          graph.push_str(&format!("{iin}label=\"{citekey} ({et}){aliases}\";\n"));
          graph.push_str(&format!("{iin}tooltip=\"{citekey} ({et})\";\n"));
          graph.push_str(&format!("{iin}fillcolor=\"{c}\";\n"));
          graph.push_str("\n");
          // NOTE: already sorted
          for field in be.datafields()) {
            graph.push_str(
              &format!("{}\"section{secnum}/{citekey}/{field}\" [ label=\"{}\" ]\n", i.repeat($in), field.to_uppercase())
            );
          }
          i_n -= 2;
          graph.push_str(&format!("{}}}\n\n", i.repeat($in)));

          // link node for cluster->cluster links
          let middle = be.count_datafields() / 2;
          $state->{$secnum}{$citekey}{linknode} = be.datafields()[middle];

        }
        else { // Granularity is at the level of entries
          graph.push_str(
            &format!("{}\"section{secnum}/{citekey}\" [ label=\"{citekey} ({et}){aliases}\", fillcolor=\"{c}\", tooltip=\"{citekey} ({et})\" ]\n", i.repeat($in))
          );

        }


        // Close set subgraph if necessary
        if (let $sets = crate::Config->get_graph("set")) {
          if ($sets->{memtoset}{$citekey}) { // entry is a set member
            graph.push_str(&format!("{}}}\n\n", i.repeat($in)));
            i_n -= 2;
          }
        }
      }

      // Then add the requested links

      // crossrefs
      if $gopts->{crossref} {
        _graph_inheritance("crossref", $secnum);
      }

      // xdata
      if $gopts->{xdata} {
        _graph_inheritance("xdata", $secnum);
      }

      // xref
      if $gopts->{xref} {
        _graph_xref($secnum);
      }

      // related
      if $gopts->{related} {
        _graph_related($secnum);
      }

      // Close the section, if any
      if ($gopts->{section}) {
        graph.push_str(&format!("{}}}\n\n", i.repeat($in)));
        $in -= 2;
      }

    }

    graph.push_str(&format!("\n\n{graph_edges}"));

    graph.push_str("\n}\n");

    out($target, $graph);

    info!("Output to {}", target_string);
    close $target;
    return;
  }

  // Graph related entries
  fn _graph_related(secnum) {
    if (let $gr = crate::Config->get_graph("related")) {

      // related links
      for f_entry in (sort keys $gr->{clonetotarget}->%*) {
        let $m = $gr->{clonetotarget}{$f_entry};
        for t_entry in (sort keys $m->%*) {
          if !($state->{$secnum}{format!("{secnum}/{f_entry}")}) {
            continue;
          }
          if !($state->{$secnum}{format!("{secnum}/{t_entry}")}) {
            continue;
          }

          if ($gopts->{field}) { // links between clusters
            let $f_linknode = $state->{$secnum}{$f_entry}{linknode};
            let $t_linknode = $state->{$secnum}{$t_entry}{linknode};
            graph_edges.push_str(
              &format!("{}\"section{secnum}/{f_entry}/{f_linknode}\" -> \"section{secnum}/{t_entry}/{t_linknode}\" [ penwidth=\"2.0\", color=\"#ad1741\", ltail=\"cluster_section{secnum}/{f_entry}\", lhead=\"cluster_section{secnum}/{t_entry}\", tooltip=\"{f_entry} is a related entry of {t_entry}\" ]\n", i.repeat($in))
            );
          }
          else {  // links between nodes
            graph_edges.push_str(
              &format!("{}\"section{secnum}/{f_entry}\" -> \"section{secnum}/{t_entry}\" [ penwidth=\"2.0\", color=\"#ad1741\", tooltip=\"{f_entry} is a related entry of {t_entry}\" ]\n", i.repeat($in))
            );
          }
        }
      }

      // clone links
      for f_entry in (sort keys $gr->{reltoclone}->%*) {
        let $m = $gr->{reltoclone}{$f_entry};
        for t_entry in (sort keys $m->%*) {
          if !($state->{$secnum}{format!("{secnum}/{f_entry}")}) {
            continue;
          }
          if !($state->{$secnum}{format!("{secnum}/{t_entry}")}) {
            continue;
          }

          if ($gopts->{field}) { // links between clusters
            let $f_linknode = $state->{$secnum}{$f_entry}{linknode};
            let $t_linknode = $state->{$secnum}{$t_entry}{linknode};
            graph_edges.push_str(
              &format!("{}\"section{secnum}/{f_entry}/{f_linknode}\" -> \"section{secnum}/{t_entry}/{t_linknode}\" [ style=\"dashed\", penwidth=\"2.0\", color=\"#ad1741\", ltail=\"cluster_section{secnum}/{f_entry}\", lhead=\"cluster_section{secnum}/{t_entry}\", tooltip=\"{t_entry} is a clone of {f_entry}\" ]\n", i.repeat($in))
            );
          }
          else {  // links between nodes
            graph_edges.push_str(
              &format!("{}\"section{secnum}/{f_entry}\" -> \"section{secnum}/{t_entry}\" [ style=\"dashed\", penwidth=\"2.0\", color=\"#ad1741\", tooltip=\"{t_entry} is a clone of {f_entry}\" ]\n", i.repeat($in))
            );
          }
        }
      }
    }
  }

  // Graph xrefs
  fn _graph_xref(secnum) {
    if (let $gr = crate::Config->get_graph("xref")) {
      for f_entry in (sort keys $gr->%*) {
        let $t_entry = $gr->{$f_entry};
        if !($state->{$secnum}{format!("{secnum}/{f_entry}")}) {
          continue;
        }
        if !($state->{$secnum}{format!("{secnum}/{t_entry}")}) {
          continue;
        }

        if ($gopts->{field}) { // links between clusters
          let $f_linknode = $state->{$secnum}{$f_entry}{linknode};
          let $t_linknode = $state->{$secnum}{$t_entry}{linknode};
          graph_edges.push_str(
            &format!("{}\"section{secnum}/{f_entry}/{f_linknode}\" -> \"section{secnum}/{t_entry}/{t_linknode}\" [ penwidth=\"2.0\", style=\"dashed\", color=\"#7d7879\", ltail=\"cluster_section{secnum}/{f_entry}\", lhead=\"cluster_section{secnum}/{t_entry}\", tooltip=\"{f_entry} XREFS {t_entry}\" ]\n", i.repeat($in))
          );
        }
        else {    // links between nodes
          graph_edges.push_str(
            &format!("{}\"section{secnum}/{f_entry}\" -> \"section{secnum}/{t_entry}\" [ penwidth=\"2.0\", style=\"dashed\", color=\"#7d7879\", tooltip=\"{f_entry} XREFS {t_entry}\" ]\n", i.repeat($in))
          );
        }
      }
    }
  }

  // Graph crossrefs and xdata
  fn _graph_inheritance(type, secnum) {
    let edgecolor = if ($type == "crossref") {
      "#7d7879"
    }
    else if ($type == "xdata") {
      "#2ca314"
    };

    if (let $gr = crate::Config->get_graph($type)) {
      // Show fields
      if ($gopts->{field}) {
        for (f_entry, v) in gr.iter().sorted_by_key(|(f_entry, _) f_entry|) {
          for (f_field, w) in v.iter().sorted_by_key(|(f_field, _) f_field|) {
            for (t_entry, val) in w.iter().sorted_by_key(|(t_entry, _) t_entry|) {
              for t_field in val {
                if !$state->{$secnum}{format!("{secnum}/{f_entry}")} {
                  continue;
                }
                if !$state->{$secnum}{format!("{secnum}/{t_entry}")} {
                  continue;
                }
                graph_edges.push_str(
                  &format!("{}\"section{secnum}/{f_entry}/{f_field}\" -> \"section{secnum}/{t_entry}/{t_field}\" [ penwidth=\"2.0\", color=\"{edgecolor}\", tooltip=\"{t_entry}/{} inherited via {} from {f_entry}/{}\" ]\n", i.repeat($in), t_field.to_uppercase(), $type.to_uppercase(), f_field.to_uppercase())
                );
              }
            }
          }
        }
      }
      // Just show the entries, no fields
      else {
        for (f_entry, v) in gr.iter().sorted_by_key(|(f_entry, _) f_entry|) {
          for (f_field, w) in v.iter().sorted_by_key(|(f_field, _) f_field|) {
            for t_entry in w.keys().sorted() {
              if !$state->{$secnum}{format!("{secnum}/{f_entry}")} {
                continue;
              }
              if !$state->{$secnum}{format!("{secnum}/{t_entry}")} {
                continue;
              }
              if $state->{edges}{format!("section{secnum}/{f_entry}")}{format!("section{secnum}/{t_entry}")} {
                continue;
              }
              graph_edges.push_str(
                &format!("{}\"section{secnum}/{f_entry}\" -> \"section{secnum}/{t_entry}\" [ penwidth=\"2.0\", color=\"{edgecolor}\", tooltip=\"{t_entry} inherits via {type} from {f_entry}\" ]\n", i.repeat($in))
              );
              $state->{edges}{format!("section{secnum}/{f_entry}")}{format!("section{secnum}/{t_entry}")} = 1;
            }
          }
        }
      }
    }
  }
}
