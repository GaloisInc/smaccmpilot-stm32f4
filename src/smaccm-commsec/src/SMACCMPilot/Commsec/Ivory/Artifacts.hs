
module SMACCMPilot.Commsec.Ivory.Artifacts
  ( commsecArtifacts
  ) where

import Ivory.Artifact
import qualified Paths_smaccm_commsec
import System.FilePath

commsecArtifacts :: [Located Artifact]
commsecArtifacts = map (\ p -> loc p $ src p)
  (aesFiles ++ curve25519Files ++ ed25519Files ++ apiFiles)
  where
  loc f = case takeExtension f of
    ".h" -> Incl
    ".c" -> Src
    _ -> Root
  src f = artifactCabalFile Paths_smaccm_commsec.getDataDir ("support" </> f)


apiFiles, aesFiles, curve25519Files, ed25519Files :: [FilePath]

apiFiles =
  [ "gec.c"
  , "gec.h"
  , "gec-ke.c"
  , "gec-ke.h"
  ]

aesFiles = map ("aes" </>)
  [ "aes.h"
  , "aescrypt.c"
  , "aeskey.c"
  , "aes_modes.c"
  , "aestab.c"
  , "aestab.h"
  , "aesopt.h"
  , "brg_endian.h"
  , "brg_types.h"
  , "gcm.c"
  , "gcm.h"
  , "gf128mul.c"
  , "gf128mul.h"
  , "gf_convert.c"
  , "gf_mul_lo.h"
  , "mode_hdr.h"
  ]

ed25519Files = map ("ed25519" </>)
  [ "ed25519.c"
  , "ed25519.h"
  , "ed25519-hash.h"
  , "ed25519-donna.h"
  , "ed25519-donna-impl-base.h"
  , "ed25519-donna-32bit-sse2.h"
  , "ed25519-donna-32bit-tables.h"
  , "ed25519-donna-64bit-x86.h"
  , "ed25519-donna-64bit-sse2.h"
  , "ed25519-donna-64bit-x86-32bit.h"
  , "ed25519-donna-64bit-tables.h"
  , "ed25519-donna-basepoint-table.h"
  , "ed25519-donna-portable.h"
  , "ed25519-donna-portable-identify.h"
  , "curve25519-donna-sse2.h"
  , "curve25519-donna-32bit.h"
  , "curve25519-donna-64bit.h"
  , "curve25519-donna-helpers.h"
  , "modm-donna-32bit.h"
  , "modm-donna-64bit.h"
  ]

curve25519Files = map ("curve25519" </>)
  [ "curve25519-donna.c"
  , "curve25519-donna.h"
  ]
