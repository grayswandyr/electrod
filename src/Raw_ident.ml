(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2024 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

type t = { ident : string; loc : Location.t }

let ident ident begp endp =
  let loc = Location.from_positions begp endp in
  { ident; loc }

let basename { ident; _ } = ident
let location { loc; _ } = loc
let eq_name i1 i2 = String.equal i1.ident i2.ident
let pp out { ident; _ } = Fmtc.string out ident

module Strings = struct
  let univ = "univ"
  let iden = "iden"
  let integers = "ints"
  let shl = "Int##SHL"
  let shr = "Int##SHR"
  let sha = "Int##SHA"
end
