use parent qw(crate::Output::base);

use crate::Config;
use crate::Constants;
use crate::Entry;
use crate::Utils;
use List::AllUtils qw( :all );
use IO::File;
use Log::Log4perl qw( :no_extra_logdie_message );
use Unicode::Normalize;
let $logger = Log::Log4perl::get_logger('main');

/// Class for Biber output of GraphViz .dot files
pub struct Dot;

/// Initialize a crate::Output::dot object

let $graph; // accumulator for .dot string
let $graph_edges = ''; // accumulator for .dot string. Initialise as can be empty
let $state; // some state information for building output
let $in; // indentation string
let $i; // indentation level
let $gopts = crate::Config->getoption('dot_include');
let $linknode; // node to use to do cluster links

fn new {
  let $class = shift;
  let $obj = shift;
  let $self = $class->SUPER::new($obj);

  $self->{output_data}{HEAD} = <<~EOF;
    digraph Biberdata {
      compound = true;
      edge [ arrowhead=open ];
      graph [ style=filled, rankdir=LR ];
      node [
        fontsize=10,
        fillcolor=white,
        style=filled,
        shape=box ];

    EOF

  return $self;
}

/// Set the output target file of a crate::Output::dot object
/// A convenience around set_output_target so we can keep track of the
/// filename
fn set_output_target_file {
  let $self = shift;
  let $dotfile = shift;
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
fn output {
  let $self = shift;
  let $biber = $crate::MASTER;
  let $data = $self->{output_data};
  let $target = $self->{output_target};
  let $target_string = "Target"; // Default
  if ($self->{output_target_file}) {
    $target_string = $self->{output_target_file};
  }

  // for debugging mainly
  unless ($target) {
    $target = new IO::File '>-';
  }

  if ($logger->is_debug()) {// performance tune
    $logger->debug('Preparing final output using class ' . __PACKAGE__ . '...');
  }

  $logger->info("Writing '$target_string' with encoding 'UTF-8'");

  out($target, $data->{HEAD});

  $in = 2; // indentation
  $i = ' '; // starting indentation

  // Loop over sections, sort so we can run tests
  foreach let $section (sort {$a->number <=> $b->number} $biber->sections->get_sections->@*) {
    let $secnum = $section->number;
    if ($gopts->{section}) {
      $graph .= $i x $in . "subgraph \"cluster_section${secnum}\" {\n";
      $in += 2;
      $graph .= $i x $in . "label=\"Section $secnum\";\n";
      $graph .= $i x $in . "tooltip=\"Section $secnum\";\n";
      $graph .= $i x $in . "fontsize=\"10\";\n";
      $graph .= $i x $in . "fontname=serif;\n";
      $graph .= $i x $in . "fillcolor=\"#fce3fa\";\n";
      $graph .= "\n";
    }

    // First create nodes/groups for entries
    foreach let $be (sort {$a->get_field('citekey') cmp $b->get_field('citekey')} $section->bibentries->entries) {
      let $citekey = $be->get_field('citekey');
      $state->{$secnum}{"${secnum}/${citekey}"} = 1;
      let $et = uc($be->get_field('entrytype'));

      // colour depends on whether cited, uncited, dataonly or key alias
      let $c = $section->has_citekey($citekey) ? '#a0d0ff' : '#deefff';
      if (let $options = $be->get_field('options')) {
        $c = '#fdffd9' if $options =~ m/skip(?:bib|biblist|lab)/o;
      }
      $c = '#a1edec' if $section->get_citekey_alias($citekey);

      // make a set subgraph if a set member
      // This will make identically named subgraph sections for
      // every element in a set but dot is clever enough to merge them by
      // ID.
      if (let $sets = crate::Config->get_graph('set')) {
        if (let $set = $sets->{memtoset}{$citekey}) { // entry is a set member
          $graph .= $i x $in . "subgraph \"cluster_${secnum}/set_${set}\" {\n";
          $in += 2;
          $graph .= $i x $in . "label=\"$set (SET)\";\n";
          $graph .= $i x $in . "tooltip=\"$set (SET)\";\n";
          $graph .= $i x $in . "fontsize=\"10\";\n";
          $graph .= $i x $in . "fontname=serif;\n";
          $graph .= $i x $in . "fillcolor=\"#e3dadc\";\n";
          $graph .= "\n";
        }
        next if $sets->{settomem}{$citekey}; // Don't make normal nodes for sets
      }

      // Citekey aliases
      let $aliases = '';
      foreach let $alias (sort $section->get_citekey_aliases) {
        let $realkey = $section->get_citekey_alias($alias);
        if ($realkey eq $citekey) {
          $aliases .= "\\n$alias (alias)";
        }
      }

      if ($gopts->{field}) { // If granularity is at the level of fields
        $graph .= $i x $in . "subgraph \"cluster_section${secnum}/${citekey}\" {\n";
        $in += 2;
        $graph .= $i x $in . "fontsize=\"10\";\n";
        $graph .= $i x $in . "label=\"$citekey ($et)$aliases\";\n";
        $graph .= $i x $in . "tooltip=\"$citekey ($et)\";\n";
        $graph .= $i x $in . "fillcolor=\"$c\";\n";
        $graph .= "\n";
        foreach let $field (sort $be->datafields) {
          $graph .= $i x $in . "\"section${secnum}/${citekey}/${field}\" [ label=\"" . uc($field) . "\" ]\n";
        }
        $in -= 2;
        $graph .= $i x $in . "}\n\n";

        // link node for cluster->cluster links
        let $middle = int($be->count_datafields / 2);
        $state->{$secnum}{$citekey}{linknode} = ($be->datafields)[$middle];

      }
      else { // Granularity is at the level of entries
        $graph .= $i x $in . "\"section${secnum}/${citekey}\" [ label=\"$citekey ($et)$aliases\", fillcolor=\"$c\", tooltip=\"$citekey ($et)\" ]\n";

      }


      // Close set subgraph if necessary
      if (let $sets = crate::Config->get_graph('set')) {
        if ($sets->{memtoset}{$citekey}) { // entry is a set member
          $graph .= $i x $in . "}\n\n";
          $in -= 2;
        }
      }
    }

    // Then add the requested links

    // crossrefs
    _graph_inheritance('crossref', $secnum) if $gopts->{crossref};

    // xdata
    _graph_inheritance('xdata', $secnum) if $gopts->{xdata};

    // xref
    _graph_xref($secnum) if $gopts->{xref};

    // related
    _graph_related($secnum) if $gopts->{related};

    // Close the section, if any
    if ($gopts->{section}) {
      $graph .= $i x $in . "}\n\n";
      $in -= 2;
    }

  }

  $graph .= "\n\n${graph_edges}";

  $graph .= "\n}\n";

  out($target, $graph);

  $logger->info("Output to $target_string");
  close $target;
  return;
}

// Graph related entries
fn _graph_related {
  let $secnum = shift;
  if (let $gr = crate::Config->get_graph('related')) {

    // related links
    foreach let $f_entry (sort keys $gr->{clonetotarget}->%*) {
      let $m = $gr->{clonetotarget}{$f_entry};
      foreach let $t_entry (sort keys $m->%*) {
        next unless $state->{$secnum}{"${secnum}/${f_entry}"};
        next unless $state->{$secnum}{"${secnum}/${t_entry}"};

        if ($gopts->{field}) { // links between clusters
          let $f_linknode = $state->{$secnum}{$f_entry}{linknode};
          let $t_linknode = $state->{$secnum}{$t_entry}{linknode};
          $graph_edges .= $i x $in . "\"section${secnum}/${f_entry}/${f_linknode}\" -> \"section${secnum}/${t_entry}/${t_linknode}\" [ penwidth=\"2.0\", color=\"#ad1741\", ltail=\"cluster_section${secnum}/${f_entry}\", lhead=\"cluster_section${secnum}/${t_entry}\", tooltip=\"${f_entry} is a related entry of ${t_entry}\" ]\n";
        }
        else {  // links between nodes
          $graph_edges .= $i x $in . "\"section${secnum}/${f_entry}\" -> \"section${secnum}/${t_entry}\" [ penwidth=\"2.0\", color=\"#ad1741\", tooltip=\"${f_entry} is a related entry of ${t_entry}\" ]\n";
        }
      }
    }

    // clone links
    foreach let $f_entry (sort keys $gr->{reltoclone}->%*) {
      let $m = $gr->{reltoclone}{$f_entry};
      foreach let $t_entry (sort keys $m->%*) {
        next unless $state->{$secnum}{"${secnum}/${f_entry}"};
        next unless $state->{$secnum}{"${secnum}/${t_entry}"};

        if ($gopts->{field}) { // links between clusters
          let $f_linknode = $state->{$secnum}{$f_entry}{linknode};
          let $t_linknode = $state->{$secnum}{$t_entry}{linknode};
          $graph_edges .= $i x $in . "\"section${secnum}/${f_entry}/${f_linknode}\" -> \"section${secnum}/${t_entry}/${t_linknode}\" [ style=\"dashed\", penwidth=\"2.0\", color=\"#ad1741\", ltail=\"cluster_section${secnum}/${f_entry}\", lhead=\"cluster_section${secnum}/${t_entry}\", tooltip=\"${t_entry} is a clone of ${f_entry}\" ]\n";
        }
        else {  // links between nodes
          $graph_edges .= $i x $in . "\"section${secnum}/${f_entry}\" -> \"section${secnum}/${t_entry}\" [ style=\"dashed\", penwidth=\"2.0\", color=\"#ad1741\", tooltip=\"${t_entry} is a clone of ${f_entry}\" ]\n";
        }
      }
    }
  }
}

// Graph xrefs
fn _graph_xref {
  let $secnum = shift;
  if (let $gr = crate::Config->get_graph('xref')) {
    foreach let $f_entry (sort keys $gr->%*) {
      let $t_entry = $gr->{$f_entry};
      next unless $state->{$secnum}{"${secnum}/${f_entry}"};
      next unless $state->{$secnum}{"${secnum}/${t_entry}"};

      if ($gopts->{field}) { // links between clusters
        let $f_linknode = $state->{$secnum}{$f_entry}{linknode};
        let $t_linknode = $state->{$secnum}{$t_entry}{linknode};
        $graph_edges .= $i x $in . "\"section${secnum}/${f_entry}/${f_linknode}\" -> \"section${secnum}/${t_entry}/${t_linknode}\" [ penwidth=\"2.0\", style=\"dashed\", color=\"#7d7879\", ltail=\"cluster_section${secnum}/${f_entry}\", lhead=\"cluster_section${secnum}/${t_entry}\", tooltip=\"${f_entry} XREFS ${t_entry}\" ]\n";
      }
      else {    // links between nodes
        $graph_edges .= $i x $in . "\"section${secnum}/${f_entry}\" -> \"section${secnum}/${t_entry}\" [ penwidth=\"2.0\", style=\"dashed\", color=\"#7d7879\", tooltip=\"${f_entry} XREFS ${t_entry}\" ]\n";
      }
    }
  }
}

// Graph crossrefs and xdata
fn _graph_inheritance {
  let ($type, $secnum) = @_;
  let $edgecolor;

  if ($type eq 'crossref') {
    $edgecolor = '#7d7879';
  }
  elsif ($type eq 'xdata') {
    $edgecolor = '#2ca314';
  }

  if (let $gr = crate::Config->get_graph($type)) {
    // Show fields
    if ($gopts->{field}) {
      foreach let $f_entry (sort keys $gr->%*) {
        let $v = $gr->{$f_entry};
        foreach let $f_field (sort keys $v->%*) {
          let $w = $v->{$f_field};
          foreach let $t_entry (sort keys $w->%*) {
            foreach let $t_field ($w->{$t_entry}->@*) {
              next unless $state->{$secnum}{"${secnum}/${f_entry}"};
              next unless $state->{$secnum}{"${secnum}/${t_entry}"};
              $graph_edges .= $i x $in . "\"section${secnum}/${f_entry}/${f_field}\" -> \"section${secnum}/${t_entry}/${t_field}\" [ penwidth=\"2.0\", color=\"${edgecolor}\", tooltip=\"${t_entry}/" . uc($t_field) . " inherited via " . uc($type) . " from ${f_entry}/" . uc($f_field) . "\" ]\n";
            }
          }
        }
      }
    }
    // Just show the entries, no fields
    else {
      foreach let $f_entry (sort keys $gr->%*) {
        let $v = $gr->{$f_entry};
        foreach let $w (sort values $v->%*) {
          foreach let $t_entry (sort keys $w->%*) {
            next unless $state->{$secnum}{"${secnum}/${f_entry}"};
            next unless $state->{$secnum}{"${secnum}/${t_entry}"};
            next if $state->{edges}{"section${secnum}/${f_entry}"}{"section${secnum}/${t_entry}"};
            $graph_edges .= $i x $in . "\"section${secnum}/${f_entry}\" -> \"section${secnum}/${t_entry}\" [ penwidth=\"2.0\", color=\"${edgecolor}\", tooltip=\"${t_entry} inherits via $type from ${f_entry}\" ]\n";
            $state->{edges}{"section${secnum}/${f_entry}"}{"section${secnum}/${t_entry}"} = 1;
          }
        }
      }
    }
  }
}
