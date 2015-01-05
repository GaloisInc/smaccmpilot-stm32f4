
module SMACCMPilot.Commsec.Ivory.Artifacts
  ( commsecArtifacts
  ) where

import qualified Paths_smaccm_commsec
import Ivory.Artifact

commsecArtifacts :: [Artifact]
commsecArtifacts = map
  (\p -> artifactCabalFile Paths_smaccm_commsec.getDataDir ("support/" ++ p))
  [ "aes.h"
  , "aescrypt.c"
  , "aeskey.c"
  , "aestab.c"
  , "aestab.h"
  , "aesopt.h"
  , "brg_endian.h"
  , "brg_types.h"
  , "commsec.c"
  , "commsec.h"
  , "gcm.c"
  , "gcm.h"
  , "gf128mul.c"
  , "gf128mul.h"
  , "gf_convert.c"
  , "gf_mul_lo.h"
  , "mode_hdr.h"
  ]
