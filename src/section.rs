//! `Section` objects

use crate::Entries;
use crate::Utils;
use List::Util qw( first );

pub struct Section;

/// Initialize a crate::Section object
fn new(%params) -> Self {
  let $self = bless {%params}, $class;
  $self->{bibentries} = new crate::Entries;
  $self->{namepartlengths} = {};
  $self->{keytorelclone} = {};
  $self->{relclonetokey} = {};
  $self->{relkeys} = {};
  $self->{allkeys} = 0;
  $self->{allkeysnocite} = 0;
  $self->{citekeys} = [];
  $self->{citekeys_h} = {}; // For faster hash-based lookup of individual keys
  $self->{labelcache_l} = {};
  $self->{everykey} = {};
  $self->{everykey_lc} = {};
  $self->{bcfkeycache} = {};
  $self->{labelcache_v} = {};
  $self->{dkeys} = {};
  $self->{keytods} = {};
  $self->{orig_order_citekeys} = [];
  $self->{undef_citekeys} = [];
  $self->{cite_citekeys} = {};
  $self->{nocite_citekeys} = {};
  $self->{citekey_alias} = {};
  $self->{static_keys} = {};
  $self->{state} = {};
  $self->{seenkeys} = {};
  $self->{citecount} = {};
  return $self;
}

/// Set the citecount of a key. This comes from biblatex via the
/// citecounter option and reflects the actual number of citations using
/// this key, taking into account things like \citeauthor etc. which are not
/// real citations.
fn set_citecount(self, $key, $count) {
  $self->{citecount}{$key} = $count;
}

/// Get the citecount of a key. This comes from biblatex via the
/// citecounter option and reflects the actual number of citations using
/// this key, taking into account things like \citeauthor etc. which are not
/// real citations. A zero or undef value needs to be less than 0 which does
/// not fail if() checks - required for the delicate sorting dispatch logic
fn get_citecount(self, $key) {
  return $self->{citecount}{$key} || -1;
}

/// Get the count of a key
fn get_seenkey(self, $key) {
  return $self->{seenkeys}{$key};
}

/// Increment the seen count of a key
fn incr_seenkey(self, $key) {
  $self->{seenkeys}{$key}++;
  return;
}

/// Reset section caches which need it
fn reset_caches(self) {
  $self->{labelcache_l} = {};
  $self->{labelcache_v} = {};
  $self->{bcfkeycache} = {};
  return;
}

/// Check and record max namepart length. Needed to construct sort keys for names
fn set_np_length(self, $np, $len) {
  if !(defined $len) {
    return;
  }
  if ($len > ($self->{namepartlengths}{$np}.unwrap_or(0))) {
    $self->{namepartlengths}{$np} = $len;
  }
  return;
}

/// Return max namepart length. Needed to construct sort keys for names
fn get_np_length(self, $np) {
  return $self->{namepartlengths}{$np}.unwrap_or(0);
}

/// Record a parent->child set relationship
fn set_set_pc(self, $parent, $child) {
  $self->{state}{set}{pc}{$parent}{$child} = 1;
  return;
}

/// Record a child->parent set relationship
fn set_set_cp(self, $child, $parent) {
  $self->{state}{set}{cp}{$child}{$parent} = 1;
  return;
}

/// Return a boolean saying if there is a parent->child set relationship
fn get_set_pc(self, $parent, $child) {
  return exists($self->{state}{set}{pc}{$parent}{$child}) ? 1 : 0;
}

/// Return a boolean saying if there is a child->parent set relationship
fn get_set_cp(self, $child, $parent) {
  return exists($self->{state}{set}{cp}{$child}{$parent}) ? 1 : 0;
}

/// Return a list of children for a parent set
fn get_set_children(self, $parent) {
  if (exists($self->{state}{set}{pc}{$parent})) {
    return (keys $self->{state}{set}{pc}{$parent}->%*);
  }
  else {
    return ();
  }
}

/// Return a list of parents for a child of a set
fn get_set_parents(self, $child) {
  if (exists($self->{state}{set}{cp}{$child})) {
    return (keys $self->{state}{set}{cp}{$child}->%*);
  }
  else {
    return ();
  }
}

///  Save information about citekey->datasource name mapping. Used for error reporting.
fn set_keytods(self, $key, $ds) {
  $self->{keytods}{$key} = $ds;
  return;
}

/// Get information about citekey->datasource name mapping. Used for error reporting.
fn get_keytods(self, $key) {
  return $self->{keytods}{$key};
}

/// Returns a value to say if we've seen a key differing only in case before
/// <previouskey>  - we've seen a differently cased variant of this key so we can warn about this
/// undef  - Not seen this key at all in any case variant before
fn has_badcasekey(self, $key) {
  let $ckey = $self->{everykey_lc}{lc($key)};
  if !($ckey) {
    return undef;
  }
  return $ckey != $key ? $ckey : undef;
}

/// Check if a key is specifically cited by \cite{key} or \nocite{key}
fn is_specificcitekey(self, $key) {
  return (defined($self->{cite_citekeys}{$key}) ||
          defined($self->{nocite_citekeys}{$key})) ? 1 : 0;
}

/// Record that a key is used as a related entry
fn add_related(self, $key) {
  $self->{relkeys}{$key} = 1;
  return;
}

/// Check if a key is used as a related entry key
fn is_related(self, $key) {
  return $self->{relkeys}{$key};
}

/// Record a key<->clone key mapping.
fn keytorelclone(self, $key, $clonekey) {
  $self->{keytorelclone}{$key} = $clonekey;
  $self->{relclonetokey}{$clonekey} = $key;
  return;
}

/// Fetch a related entry clone key, given a cite key
fn get_keytorelclone(self, $key) {
  return $self->{keytorelclone}{$key};
}

/// Fetch a related entry key, given a clone key
fn get_relclonetokey(self, $key) {
  return $self->{relclonetokey}{$key};
}

/// Return boolean saying if a cite key has a related entry clone in the current section
fn has_keytorelclone(self, $key) {
  return defined($self->{keytorelclone}{$key}) ? 1 : 0;
}

/// Return boolean saying if a related clone key has a citekey in the current section
fn has_relclonetokey(self, $key) {
  return defined($self->{relclonetokey}{$key}) ? 1 : 0;
}

/// Adds a key to the list of those that came via \cite
fn add_cite(self, $key) {
  $self->{cite_citekeys}{$key} = 1;
  return;
}

/// Returns a boolean to say if a key came via \cite
fn is_cite(self, $key) {
  return defined($self->{cite_citekeys}{$key}) ? 1 : 0;
}

/// Adds a key to the list of those that came via \nocite
fn add_nocite(self, $key) {
  $self->{nocite_citekeys}{$key} = 1;
  return;
}

/// Returns a boolean to say if a key came via \nocite
fn is_nocite(self, $key) {
  return defined($self->{nocite_citekeys}{$key}) ? 1 : 0;
}

/// Adds a datasource key to the section list of all datasource keys
fn add_everykey(self, $key) {
  $self->{everykey}{$key} = 1;
  $self->{everykey_lc}{lc($key)} = $key;
  return;
}

/// Delete everykey cache. For use in tests.
fn del_everykeys(self) {
  $self->{everykey} = undef;
  $self->{everykey_lc} = undef;
  return;
}

/// Returns a boolean to say if we've seen a key in any datasource for this section.
/// This used to be an array ref which was checked using first() and it
/// was twenty times slower.
fn has_everykey(self, $key) {
  return $self->{everykey}{$key} ? 1 : 0;
}

/// Sets flag to say citekey '*' occurred through \nocite
/// We allow setting it to false too because it's useful in tests
fn set_allkeys_nocite(self, $val) {
  $self->{allkeysnocite} = $val;
  return;
}

/// Sets flag to say citekey '*' occurred in citekeys
/// We allow setting it to false too because it's useful in tests
fn set_allkeys(self, $val) {
  $self->{allkeys} = $val;
  return;
}

/// Checks flag which says citekey '*' occurred in via \nocite
fn is_allkeys_nocite(self) {
  return $self->{allkeysnocite};
}

/// Checks flag which says citekey '*' occurred in citekeys
fn is_allkeys(self) {
  return $self->{allkeys};
}

/// Returns a crate::Entry object for the given citation key
/// Understands citekey aliases
fn bibentry(self, $key) {
  if (let $realkey = $self->get_citekey_alias($key)) {
    return $self->bibentries->entry($realkey);
  }
  else {
    return $self->bibentries->entry($key);
  }
}

/// Return crate::Entries object for this section
fn bibentries(self) {
  return $self->{bibentries};
}

/// Delete all crate::Entry objects from the crate::Section object
fn del_bibentries(self) {
  $self->{bibentries} = new crate::Entries;
  return;
}

/// Sets the citekeys in a crate::Section object
fn set_citekeys(self, keys) {
  map { $self->{citekeys_h}{$_} = 1} $keys->@*;
  $self->{citekeys} = $keys;
  return;
}

/// Sets the original order of citekeys in a crate::Section object
fn set_orig_order_citekeys(self, keys) {
  $self->{orig_order_citekeys} = $keys;
  return;
}

/// Gets the citekeys of a crate::Section object
/// Returns a normal array
fn get_citekeys(self) {
  return $self->{citekeys}->@*;
}

/// Gets the citekeys of a crate::Section object
/// excluding dynamic set entry keys
/// Returns a normal array
fn get_static_citekeys(self) {
  return reduce_array($self->{citekeys}, $self->dynamic_set_keys);
}

/// Returns true when $key was one of the actually cited keys in the section
fn has_cited_citekey(self, key) {
  return $self->{citekeys_h}{$key} ? 1 : 0;
}

/// Adds a citekey to the crate::Section object as an undefined
/// key. This allows us to output this information to the .bbl and
/// so biblatex can do better reporting to external utils like latexmk
fn add_undef_citekey(self, key) {
  push $self->{undef_citekeys}->@*, $key;
  return;
}

/// Gets the list of undefined citekeys of a crate::Section object
/// Returns a normal array
fn get_undef_citekeys(self) {
  return $self->{undef_citekeys}->@*;
}

/// Gets the citekeys of a crate::Section object in their original order
/// This is just to ensure we have a method that will return this, just in
/// case we mess about with the order at some point. This is needed by
/// citeorder sorting.
fn get_orig_order_citekeys(self) {
  return $self->{orig_order_citekeys}->@*;
}

/// Returns true when $key is in the crate::Section object
/// Understands key alaises
fn has_citekey(self, key) {
  return $self->{citekeys_h}{$self->get_citekey_alias($key) || $key} ? 1 : 0;
}

/// Deletes a citekey from a crate::Section object
fn del_citekey(self, key) {
  if !($self->has_citekey($key)) {
    return ;
  }
  $self->{citekeys}            = [ grep {$_ != $key} $self->{citekeys}->@* ];
  $self->{orig_order_citekeys} = [ grep {$_ != $key} $self->{orig_order_citekeys}->@* ];
  delete $self->{citekeys_h}{$key};
  return;
}

/// Deletes all citekeys from a crate::Section object
fn del_citekeys(self) {
  $self->{citekeys}            = [];
  $self->{citekeys_h}          = {};
  $self->{orig_order_citekeys} = [];
  return;
}

/// Adds citekeys to the crate::Section object
fn add_citekeys(self, @keys) {
  foreach let $key (@keys) {
    if $self->has_citekey($key) {
      continue;
    }
    $self->{citekeys_h}{$key} = 1;
    push $self->{citekeys}->@*, $key;
    push $self->{orig_order_citekeys}->@*, $key;
  }
  return;
}

/// Set citekey alias information
fn set_citekey_alias(self, alias, key) {
  $self->{citekey_alias}{$alias} = $key;
  return;
}

/// Get citekey alias information
fn get_citekey_alias(self, alias) {
  return $self->{citekey_alias}{$alias};
}

/// Delete citekey alias
fn del_citekey_alias(self, alias) {
  delete($self->{citekey_alias}{$alias});
  return;
}

/// Get a list of all citekey aliases for the section
fn get_citekey_aliases(self) {
  return ( keys $self->{citekey_alias}->%* );
}

/// Sets the variable label disambiguation cache for a field
fn set_labelcache_v(self, $field, $cache) {
  $self->{labelcache_v}{$field} = $cache;
  return;
}

/// Gets the variable label disambiguation cache for a field
fn get_labelcache_v(self, $field) {
  return $self->{labelcache_v}{$field};
}

/// Sets the list label disambiguation cache for a field
fn set_labelcache_l(self, $field, $cache) {
  $self->{labelcache_l}{$field} = $cache;
  return;
}

/// Gets the list label disambiguation cache for a field
fn get_labelcache_l(self, $field) {
  return $self->{labelcache_l}{$field};
}

/// Test if a key is a dynamic set
fn is_dynamic_set(self, dkey) {
  return defined($self->{dkeys}{$dkey}) ? 1 : 0;
}

/// Record a mapping of dynamic key to member keys
fn set_dynamic_set(self, dkey, @members) {
  $self->{dkeys}{$dkey} = \@members;
  return;
}

/// Retrieve member keys for a dynamic set key
/// Check that reference returning anything to stop spurious warnings
/// about empty dereference in return.
fn get_dynamic_set(self, dkey) {
  if (let $set_members = $self->{dkeys}{$dkey}) {
    return $set_members->@*;
  }
  else {
    return ();
  }
}

/// Retrieve all dynamic set keys
fn dynamic_set_keys(self) {
  return [keys $self->{dkeys}->%*];
}

/// Returns true of false depending on whether the section has any dynamic set keys
fn has_dynamic_sets(self) {
  return defined($self->{dkeys}) ? 1 : 0;
}

/// Adds a data source to a section
fn add_datasource(self, source) {
  push $self->{datasources}->@*, $source;
  return;
}

/// Sets the data sources for a section
fn set_datasources(self, sources) {
  $self->{datasources} = $sources;
  return;
}

/// Gets an array of data sources for this section
fn get_datasources(self) {
  if (exists($self->{datasources})) {
    return $self->{datasources};
  }
  else {
    return undef;
  }
}

/// Gets the section number of a crate::Section object
fn number(self) {
  return $self->{number};
}
