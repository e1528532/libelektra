--
-- @file
--
-- @brief Errors Haskell bindings
--
-- @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
-- 
module Elektra.Errors (
  triggerError, triggerWarnings
) where

#include <hskdberrors.h>

{#import Elektra.Key#}

{#context lib="libelektra" #}

{#fun unsafe hsElektraTriggerError as triggerError {`Int', `Key', `String'} -> `()' #}
{#fun unsafe hsElektraTriggerWarnings as triggerWarnings {`Int', `Key', `String'} -> `()' #}
