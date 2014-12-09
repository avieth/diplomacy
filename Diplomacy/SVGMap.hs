{-# LANGUAGE OverloadedStrings #-}

-- Diplomacy board renderer to SVG.
-- Inspired by and largely copied from the SVG image here:
-- http://upload.wikimedia.org/wikipedia/commons/a/a3/Diplomacy.svg

import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Pretty (renderSvg)
import Text.Blaze.Internal (stringValue)

import Control.Monad (forM_)
import Data.List ((\\))

import Diplomacy.Board
import Diplomacy.Country
import Diplomacy.PlayerCount
import Diplomacy.Province

svgMap :: Board -> String
svgMap = renderSvg . preamble . svgBoard

svgBoard :: Board -> S.Svg
svgBoard board =
  forM_ (fixOrder properProvinceTargets) (provinceGroup board) >>
  forM_ (supplyCentres) (supplyCentreIcon)
-- must be drawn beneath its neighbours.
-- TODO Siwtzerland

-- | We reorder because the drawing method uses a hack: the north sea must
--   be drawn as soon as possible, but not after the norwegian sea is drawn!
fixOrder :: [ProvinceTarget] -> [ProvinceTarget]
fixOrder pts = Normal NorwegianSea : Normal NorthSea : (pts \\ [Normal NorthSea, Normal NorwegianSea])

preamble :: S.Svg -> S.Svg
preamble = S.docTypeSvg ! A.version "1.0" ! A.viewbox "0 0 610 560"

countryColour Austria = "#FF0000";
countryColour UnitedKingdom = "#0000FF"
countryColour France = "#00FFFF"
countryColour Germany = "#808080"
countryColour Italy = "#00FF00"
countryColour Russia = "#008000"
countryColour Ottoman = "#FFFF00"

provinceStyle b p =
  if isWater p
  then stringValue $ "fill: #99CCFF; stroke: black;"
  else stringValue $ "fill:" ++ colour ++ "; stroke:black;"
    where colour = maybe "#FFFFDD" countryColour (controllerOf b p)

provinceGroup :: Board -> ProvinceTarget -> S.Svg
provinceGroup b p = S.g $ do
  provinceElement p ! A.style (provinceStyle b pr)
  provinceExtras p
  provinceText pr ! A.style "font-size:8px; font-family:Arial,Helvetica,sans-serif;"
    where pr = ptProvince p

supplyCentreIcon :: Province -> S.Svg
supplyCentreIcon p = S.circle ! A.r "3" ! A.transform t
  where t = case p of
              Ankara -> S.translate 482 496
              Belgium -> S.translate 186 305
              Berlin -> S.translate 281 298
              Brest -> S.translate 106 322
              Budapest -> S.translate 326 376
              Bulgaria -> S.translate 377 444
              Constantinople -> S.translate 429 460
              Denmark -> S.translate 272 252
              Edinburgh -> S.translate 154 219
              Greece -> S.translate 378 507
              Holland -> S.translate 205 284
              Kiel -> S.translate 254 278
              Liverpool -> S.translate 144 257
              London -> S.translate 162 290
              Marseilles -> S.translate 186 417
              Moscow -> S.translate 481 234
              Munich -> S.translate 258 359
              Naples -> S.translate 278 469
              Norway -> S.translate 270 187
              Paris -> S.translate 173 334
              Portugal -> S.translate 15 434
              Rome -> S.translate 252 443
              Rumania -> S.translate 402 413
              Serbia -> S.translate 343 419
              Sevastopol -> S.translate 483 396
              Smyrna -> S.translate 424 502
              Spain -> S.translate 80 432
              StPetersburg -> S.translate 418 187
              Sweden -> S.translate 323 196
              Trieste -> S.translate 284 396
              Tunis -> S.translate 220 529
              Venice -> S.translate 261 397
              Vienna -> S.translate 301 363
              Warsaw -> S.translate 346 302
              x -> error "supplyCentreIcon must only be applied to supplyCentres"

provinceText :: Province -> S.Svg
provinceText NorwegianSea = S.text_ "NWG" ! A.x "220" ! A.y "70"
provinceText NorthSea = S.text_ "NTH" ! A.x "190" ! A.y "230"
provinceText AdriaticSea = S.text_ "ADR" ! A.x "308" ! A.y "460"
provinceText AegeanSea = S.text_ "AEG" ! A.x "392" ! A.y "510"
provinceText Albania = S.text_ "ALB" ! A.x "333" ! A.y "460"
provinceText Ankara = S.text_ "ANK" ! A.x "510" ! A.y "455"
provinceText Apulia = S.text_ "APU" ! A.x "291" ! A.y "470"
provinceText Armenia = S.text_ "ARM" ! A.x "585" ! A.y "467"
provinceText BalticSea = S.text_ "BAL" ! A.x "308" ! A.y "260"
provinceText BarentsSea = S.text_ "BAR" ! A.x "440" ! A.y "15"
provinceText Belgium = S.text_ "BEL" ! A.x "192" ! A.y "321"
provinceText Berlin = S.text_ "BER" ! A.x "272" ! A.y "292"
provinceText BlackSea = S.text_ "BLA" ! A.x "500" ! A.y "418"
provinceText Bohemia = S.text_ "BOH" ! A.x "283" ! A.y "347"
provinceText Brest = S.text_ "BRE" ! A.x "130" ! A.y "354"
provinceText Budapest = S.text_ "BUD" ! A.x "350" ! A.y "390"
provinceText Bulgaria = S.text_ "BUL" ! A.x "395" ! A.y "443"
provinceText Burgundy = S.text_ "BUR" ! A.x "185" ! A.y "371"
provinceText Clyde = S.text_ "CLY" ! A.x "133" ! A.y "201"
provinceText Constantinople = S.text_ "CON" ! A.x "435" ! A.y "483"
provinceText Denmark = S.text_ "DEN" ! A.x "250" ! A.y "235"
provinceText EasternMediterranean = S.text_ "EAS" ! A.x "455" ! A.y "550"
provinceText Edinburgh = S.text_ "EDI" ! A.x "152" ! A.y "202"
provinceText EnglishChannel = S.text_ "ENG" ! A.x "134" ! A.y "306"
provinceText Finland = S.text_ "FIN" ! A.x "375" ! A.y "160"
provinceText Galacia = S.text_ "GAL" ! A.x "355" ! A.y "343"
provinceText Gascony = S.text_ "GAS" ! A.x "130" ! A.y "400"
provinceText Greece = S.text_ "GRE" ! A.x "352" ! A.y "490"
provinceText GulfOfLyon = S.text_ "LYO" ! A.x "170" ! A.y "457"
provinceText GulfOfBothnia = S.text_ "BOT" ! A.x "328" ! A.y "175"
provinceText HelgolandBright = S.text_ "HEL" ! A.x "220" ! A.y "265"
provinceText Holland = S.text_ "HOL" ! A.x "210" ! A.y "290"
provinceText IonianSea = S.text_ "ION" ! A.x "315" ! A.y "520"
provinceText IrishSea = S.text_ "IRI" ! A.x "95" ! A.y "270"
provinceText Kiel = S.text_ "KIE" ! A.x "237" ! A.y "285"
provinceText Liverpool = S.text_ "LVP" ! A.x "138" ! A.y "230"
provinceText Livonia = S.text_ "LVN" ! A.x "380" ! A.y "260"
provinceText London = S.text_ "LON" ! A.x "160" ! A.y "280"
provinceText Marseilles = S.text_ "MAR" ! A.x "173" ! A.y "412"
provinceText MidAtlanticOcean = S.text_ "MAO" ! A.x "50" ! A.y "355"
provinceText Moscow = S.text_ "MOS" ! A.x "460" ! A.y "265"
provinceText Munich = S.text_ "MUN" ! A.x "235" ! A.y "360"
provinceText Naples = S.text_ "NAP" ! A.x "293" ! A.y "493"
provinceText NorthAtlanticOcean = S.text_ "NAO" ! A.x "65" ! A.y "120"
provinceText NorthAfrica = S.text_ "NAF" ! A.x "130" ! A.y "536"
provinceText Norway = S.text_ "NWY" ! A.x "250" ! A.y "175"
provinceText Paris = S.text_ "PAR" ! A.x "155" ! A.y "358"
provinceText Picardy = S.text_ "PIC" ! A.x "168"  ! A.y "326"
provinceText Piedmont = S.text_ "PIE" ! A.x "215" ! A.y "408"
provinceText Portugal = S.text_ "POR" ! A.x "22" ! A.y "440"
provinceText Prussia = S.text_ "PRU" ! A.x "335" ! A.y "283"
provinceText Rome = S.text_ "ROM" ! A.x "257" ! A.y "452"
provinceText Ruhr = S.text_ "RUH" ! A.x "215" ! A.y "330"
provinceText Rumania = S.text_ "RUM" ! A.x "410" ! A.y "415"
provinceText Serbia = S.text_ "SER" ! A.x "350" ! A.y "450"
provinceText Sevastopol = S.text_ "SEV" ! A.x "540" ! A.y "350"
provinceText Silesia = S.text_ "SIL" ! A.x "304" ! A.y "325"
provinceText Skagerrak = S.text_ "SKA" ! A.x "255" ! A.y "220"
provinceText Smyrna = S.text_ "SMY" ! A.x "460" ! A.y "510"
provinceText Spain = S.text_ "SPA" ! A.x "85" ! A.y "450"
provinceText StPetersburg = S.text_ "STP" ! A.x "460" ! A.y "149"
provinceText Sweden = S.text_ "SWE" ! A.x "300" ! A.y "170"
provinceText Syria = S.text_ "SYR" ! A.x "570" ! A.y "535"
provinceText Trieste = S.text_ "TRI" ! A.x "305" ! A.y "425"
provinceText Tunis = S.text_ "TUN" ! A.x "210" ! A.y "555"
provinceText Tuscany = S.text_ "TUS" ! A.x "240" ! A.y "425"
provinceText Tyrolia = S.text_ "TYR" ! A.x "255" ! A.y "380"
provinceText TyrrhenianSea = S.text_ "TYS" ! A.x "245" ! A.y "495"
provinceText Ukraine = S.text_ "UKR" ! A.x "420" ! A.y "340"
provinceText Venice = S.text_ "VEN" ! A.x "245" ! A.y "407"
provinceText Vienna = S.text_ "VIE" ! A.x "307" ! A.y "370"
provinceText Wales = S.text_ "WAL" ! A.x "130" ! A.y "275"
provinceText Warsaw = S.text_ "WAR" ! A.x "355" ! A.y "304"
provinceText WesternMediterranean = S.text_ "WES" ! A.x "160" ! A.y "491"
provinceText Yorkshire = S.text_ "YOR" ! A.x "155" ! A.y "254"

provinceElement :: ProvinceTarget -> S.Svg
provinceElement (Normal NorwegianSea) = S.polygon ! A.points "362,33 357,39 343,44 324,54 320,64 310,75 309,84 303,86 292,111 277,132 269,134 264,142 258,141 236,154 198,154 171,181 171,197 158,193 152,194 154,188 161,185 162,181 148,177 148,0 362,0"
provinceElement (Normal NorthSea) = S.path ! A.d "M171,197 L171,181 A27,27 0,0,1 198,154 L241,154 L241,224 L248,224 L245,237 L211,237 L211,301 L173,301 L165,293 L140,197Z"
provinceElement (Normal AdriaticSea) = S.polygon ! A.points "322,480 297,456 300,453 290,453 278,443 272,424 260,417 261,401 270,398 276,399 275,403 278,410 282,401 286,402 289,418 306,436 331,454 331,477 335,480"
provinceElement (Normal AegeanSea) = S.polygon ! A.points "376,537 371,520 378,521 377,513 386,516 385,509 370,494 371,491 378,494 368,483 371,477 379,484 382,483 381,477 386,478 380,472 392,472 400,468 408,470 410,473 414,475 410,482 409,487 417,486 417,489 420,495 417,498 417,507 423,510 427,524 435,523 435,530 416,549 412,547 387,546 383,544"
provinceElement (Normal Albania) = S.polygon ! A.points "331,454 331,477 335,480 339,487 350,477 350,471 346,466 346,452 337,446 330,445"
provinceElement (Normal Ankara) = S.polygon ! A.points "555,438 551,437 520,441 514,438 511,440 502,433 481,438 470,447 464,457 468,461 468,479 466,491 473,491 490,480 501,482 508,480 531,460 546,462 555,460 557,449"
provinceElement (Normal Apulia) = S.polygon ! A.points "304,484 310,480 318,485 322,485 322,480 297,456 300,453 290,453 278,443 274,447 279,451 280,455 279,458 293,481"
provinceElement (Normal Armenia) = S.polygon ! A.points "609,493 584,478 563,479 562,471 556,467 555,460 557,449 555,438 570,427 589,442 594,439 603,441 609,440"
provinceElement (Normal BalticSea) = S.polygon ! A.points "266,255 271,260 278,254 277,250 280,248 279,243 282,253 289,254 294,245 305,244 312,229 311,220 359,220 349,229 347,243 347,248 348,254 344,262 337,264 334,273 328,274 326,265 314,266 307,273 294,275 286,274 287,267 280,266 266,275 261,274 260,269 256,266 256,263 254,255"
provinceElement (Normal BarentsSea) = S.polygon ! A.points "540 0 535,9 530,6 517,19 516,33 513,38 513,23 507,20 505,26 499,33 492,48 495,58 488,60 479,57 477,55 481,50 473,43 466,45 472,62 478,66 478,74 472,72 468,74 457,91 469,100 467,106 462,109 444,101 442,110 447,115 454,119 452,122 434,118 426,103 426,94 414,88 412,83 445,84 457,79 459,66 453,61 417,47 405,49 401,45 397,48 391,47 395,41 394,38 384,33 382,40 380,33 377,31 374,38 371,33 366,42 366,33 362,33 362,0"
provinceElement (Normal Belgium) = S.polygon ! A.points "191,299 194,303 206,306 205,311 208,315 210,326 205,331 192,323 184,315 169,311 173,301"
provinceElement (Normal Berlin) = S.polygon ! A.points "294,275 286,274 287,267 280,266 266,275 266,283 262,287 264,293 261,296 263,310 288,305 296,300 297,296 292,290"
provinceElement (Normal BlackSea) = S.polygon ! A.points "440,458 430,455 426,450 422,441 425,427 429,426 430,423 432,409 439,404 438,397 446,378 459,375 461,377 459,379 465,383 476,381 478,383 472,385 468,392 477,396 477,401 486,404 488,397 494,396 497,392 507,389 506,384 494,387 485,378 503,364 526,351 527,354 514,365 517,371 520,371 515,384 511,383 510,386 517,393 528,394 554,406 567,408 573,417 570,427 555,438 551,437 520,441 514,438 511,440 502,433 481,438 470,447 464,457 442,460"
provinceElement (Normal Bohemia) = S.polygon ! A.points "281,356 276,346 268,343 264,329 266,325 278,326 288,321 297,322 311,334 314,332 321,339 322,347 316,348 303,346 295,349 292,357"
provinceElement (Normal Brest) = S.polygon ! A.points "150,319 144,318 142,312 136,310 136,326 124,323 122,318 102,317 100,322 103,328 109,329 123,344 122,350 123,357 128,363 146,365 146,337 148,329"
provinceElement (Normal Budapest) = S.polygon ! A.points "394,376 395,382 401,385 406,396 401,402 387,402 367,406 365,412 360,413 342,410 338,412 335,410 332,410 323,408 321,398 311,394 308,383 311,375 322,370 335,354 337,350 350,347 360,351 368,353 377,360 378,363 384,365"
provinceElement (Special BulgariaEast) = S.polyline ! A.points "413,464 412,454 420,451 426,450 422,441 425,427 429,426 430,423 422,420 410,420 404,422 398,427 390,425 382,427 375,423 370,425 367,421 365,425 368,433 371,438"
provinceElement (Special BulgariaSouth) = S.polyline ! A.points "371,438 366,439 371,456 365,461 369,464 376,464 388,460 392,472 400,468 408,470 413,464 412,454"
provinceElement (Normal Burgundy) = S.polygon ! A.points "192,323 205,331 204,338 211,346 213,352 209,363 208,367 194,382 178,381 178,390 173,396 168,395 163,387 165,383 158,380 156,374 165,365 185,344 188,332"
provinceElement (Normal Clyde) = S.polygon ! A.points "138,214 130,208 129,197 139,189 140,182 148,177 162,181 161,185 154,188 152,194 146,200 144,213"
provinceElement (Normal Constantinople) = S.polygon ! A.points "408,470 410,473 414,475 410,482 409,487 417,486 417,489 423,487 432,493 452,495 466,491 468,479 468,461 464,457 442,460 440,458 430,455 426,450 420,451 412,454 413,464"
provinceElement (Normal Denmark) = S.polygon ! A.points "279,243 275,242 269,243 266,240 267,234 266,221 263,223 248,224 245,237 243,247 244,254 254,255 266,255 271,260 278,254 277,250 280,248"
provinceElement (Normal EasternMediterranean) = S.polygon ! A.points "435,530 441,526 447,528 453,534 464,531 466,521 475,520 485,528 491,530 505,526 511,514 520,517 527,508 530,509 525,518 526,530 532,535 528,559 400,559 400,554 414,552 416,549"
provinceElement (Normal Edinburgh) = S.polygon ! A.points "152,194 158,193 171,197 170,202 165,210 158,214 151,215 157,216 161,218 163,226 155,228 145,217 144,213 146,200"
provinceElement (Normal EnglishChannel) = S.polygon ! A.points "173,301 169,311 153,315 155,320 150,319 144,318 142,312 136,310 136,326 124,323 122,318 102,317 88,303 100,291 110,292 120,295 124,291 134,294 147,295 160,298 168,296"
provinceElement (Normal Finland) = S.polygon ! A.points "362,107 368,108 372,120 366,121 359,136 345,151 347,160 350,165 348,178 349,184 357,186 365,191 384,185 402,177 412,161 410,152 414,147 410,130 402,118 401,110 392,92 393,73 387,68 388,61 386,58 388,54 379,48 370,49 369,61 355,62 346,54 342,61 356,71"
provinceElement (Normal Galacia) = S.polygon ! A.points "333,330 341,330 344,332 353,327 356,323 361,324 367,329 374,327 379,324 383,327 385,332 399,338 404,354 403,360 404,371 394,376 384,365 378,363 377,360 368,353 360,351 350,347 337,350 329,346 322,347 321,339 322,347 321,339 325,340 329,338"
provinceElement (Normal Gascony) = S.polygon ! A.points "128,363 121,382 122,384 112,399 113,407 123,412 134,417 135,414 142,417 149,403 157,397 168,395 163,387 165,383 158,380 156,374 149,372 146,365"
provinceElement (Normal Greece) = S.polygon ! A.points "339,487 346,498 350,498 347,500 352,508 367,507 371,511 355,510 350,514 357,521 359,533 360,528 367,536 368,531 376,537 371,520 378,521 377,513 386,516 385,509 370,494 371,491 378,494 368,483 371,477 379,484 382,483 381,477 386,478 380,472 392,472 388,460 376,464 369,464 361,467 356,471 350,471 350,477"
provinceElement (Normal GulfOfLyon) = S.polygon ! A.points "115,469 110,461 124,444 131,439 146,438 157,432 158,425 158,418 169,412 176,417 188,422 198,421 211,416 222,410 233,415 238,431 224,431 221,434 211,436 213,451 218,454 218,458 214,461 206,462 205,466 154,466 148,463 142,469"
provinceElement (Normal GulfOfBothnia) = S.polygon ! A.points "311,220 314,209 322,206 328,203 331,193 326,183 320,182 321,161 330,146 343,138 351,128 347,121 349,112 355,104 362,107 368,108 372,120 366,121 359,136 345,151 347,160 350,165 348,178 349,184 357,186 365,191 384,185 402,177 403,183 411,184 414,187 408,187 400,192 399,197 387,196 371,198 369,202 365,204 368,210 372,213 373,221 377,227 373,229 366,228 359,220"
provinceElement (Normal HelgolandBright) = S.polygon ! A.points "245,237 243,247 244,254 243,257 245,263 244,270 244,273 235,277 234,274 230,273 226,275 211,274 211,237"
provinceElement (Normal Holland) = S.polygon ! A.points "226,275 227,280 225,292 220,298 215,297 213,302 210,313 208,315 205,311 206,306 194,303 191,299 198,289 205,276 205,279 207,279 211,274"
provinceElement (Normal IonianSea) = S.polygon ! A.points "289,511 290,514 295,515 308,500 311,491 304,484 310,480 318,485 322,485 322,480 335,480 339,487 346,498 350,498 347,500 352,508 367,507 371,511 355,510 350,514 357,521 359,533 360,528 367,536 368,531 376,537 383,544 380,547 383,550 400,554 400,559 232,559 234,551 232,544 225,535 231,531 236,524 247,513 258,519 273,531 281,532 282,521 285,513 285,511"
provinceElement (Normal IrishSea) = S.polygon ! A.points "100,291 112,287 122,281 130,282 127,276 119,272 116,272 115,265 128,262 126,256 121,257 132,250 135,250 139,240 136,229 130,227 120,227 110,232 109,246 98,259 87,257 70,261 58,273 88,303"
provinceElement (Normal Kiel) = S.polygon ! A.points "244,254 243,257 245,263 244,270 244,273 235,277 234,274 230,273 226,275 227,280 225,292 220,298 215,297 213,302 232,308 241,316 243,322 263,310 261,296 264,293 262,287 266,283 266,275 261,274 260,269 256,266 256,263 254,255"
provinceElement (Normal Liverpool) = S.polygon ! A.points "128,262 126,256 121,257 132,250 135,250 139,240 136,229 130,227 130,223 138,217 138,214 144,213 145,217 155,228 155,239 151,248 150,264 143,262"
provinceElement (Normal Livonia) = S.polygon ! A.points "369,202 365,204 368,210 372,213 373,221 377,227 373,229 366,228 359,220 349,229 347,243 354,251 356,261 362,260 367,265 365,281 372,283 379,290 389,285 392,278 404,275 405,239 409,228 405,217 394,205 382,206 372,205"
provinceElement (Normal London) = S.polygon ! A.points "166,269 168,270 171,268 177,270 178,274 176,283 165,293 172,294 168,296 160,298 147,295 145,281 150,277 153,271"
provinceElement (Normal Marseilles) = S.polygon ! A.points "142,417 149,403 157,397 168,395 173,396 178,390 178,381 194,382 197,385 203,379 207,386 204,390 207,396 201,399 204,402 203,410 211,416 198,421 188,422 176,417 169,412 158,418 158,425 154,427"
provinceElement (Normal MidAtlanticOcean) = S.polygon ! A.points "102,317 100,322 103,328 109,329 123,344 122,350 123,357 128,363 121,382 122,384 112,399 101,396 96,397 72,384 59,381 54,375 48,374 46,378 39,375 33,381 35,384 32,396 30,406 17,427 14,427 10,433 13,440 15,441 12,450 13,454 8,462 19,469 27,468 33,475 34,484 37,490 37,495 33,496 17,518 0,520 0,273 58,273"
provinceElement (Normal Moscow) = S.polygon ! A.points "609,117 598,132 573,143 564,159 534,164 515,169 489,184 476,183 458,194 456,207 457,210 451,213 447,209 439,211 428,225 421,229 409,228 405,239 404,275 392,278 389,285 379,290 386,309 390,306 456,292 468,295 477,289 494,295 505,280 516,286 526,287 533,283 549,284 554,304 564,305 569,321 597,330 609,330"
provinceElement (Normal Munich) = S.polygon ! A.points "234,366 243,370 246,369 250,371 267,368 271,370 269,362 275,362 281,356 276,346 268,343 264,329 266,325 278,326 288,321 284,314 288,305 263,310 243,322 237,322 219,344 211,346 213,352 209,363 222,365 225,362 232,363"
provinceElement (Normal Naples) = S.polygon ! A.points "271,464 276,474 290,487 294,502 289,511 290,514 295,515 308,500 311,491 304,484 293,481 279,458"
provinceElement (Normal NorthAtlanticOcean) = S.polygon ! A.points "70,261 64,250 67,242 71,245 81,234 74,228 80,225 78,218 82,217 89,220 94,220 95,218 94,216 97,216 101,212 110,212 119,217 120,227 130,227 130,223 138,217 138,214 130,208 129,197 139,189 140,182 148,177 148,0 0,0 0,273 58,273"
provinceElement (Normal NorthAfrica) = S.polygon ! A.points "203,520 179,515 169,518 150,511 117,509 106,511 99,515 89,512 84,518 79,520 68,516 68,511 64,514 46,509 42,502 41,494 37,495 33,496 17,518 0,520 0,559 195,559 197,527"
provinceElement (Normal Norway) = S.polygon ! A.points "397,48 391,47 395,41 394,38 384,33 382,40 380,33 377,31 374,38 371,33 366,42 366,33 362,33 357,39 343,44 324,54 320,64 310,75 309,84 303,86 292,111 277,132 269,134 264,142 258,141 236,154 237,160 233,167 231,180 233,186 229,192 231,201 241,209 246,210 266,201 270,193 275,203 279,204 287,177 285,170 290,164 292,133 301,132 300,126 309,115 308,104 311,101 324,71 332,74 330,64 341,65 342,61 346,54 355,62 369,61 370,49 379,48 388,54 386,58 388,61"
provinceElement (Normal Paris) = S.polygon ! A.points "146,365 149,372 156,374 165,365 185,344 188,332 172,328 165,331 159,331 148,329 146,337"
provinceElement (Normal Picardy) = S.polygon ! A.points "169,311 153,315 155,320 150,319 148,329 159,331 165,331 172,328 188,332 192,323 184,315"
provinceElement (Normal Piedmont) = S.polygon ! A.points "207,386 204,390 207,396 201,399 204,402 203,410 211,416 222,410 233,415 236,411 233,404 246,392 243,388 229,385 227,390 221,385 213,387"
provinceElement (Normal Portugal) = S.polygon ! A.points "32,396 30,406 17,427 14,427 10,433 13,440 15,441 12,450 13,454 8,462 19,469 27,468 36,457 34,447 40,441 37,431 42,432 52,412 61,411 62,407 55,400 42,399 43,395"
provinceElement (Normal Prussia) = S.polygon ! A.points "347,243 347,248 348,254 344,262 337,264 334,273 328,274 326,265 314,266 307,273 294,275 292,290 297,296 296,300 320,303 324,299 326,292 341,287 345,289 359,286 365,281 367,265 362,260 356,261 354,251"
provinceElement (Normal Rome) = S.polygon ! A.points "247,442 248,447 256,458 271,464 279,458 280,455 279,451 274,447 263,434 250,438"
provinceElement (Normal Ruhr) = S.polygon ! A.points "213,302 210,313 208,315 210,326 205,331 204,338 211,346 219,344 237,322 243,322 241,316 232,308"
provinceElement (Normal Rumania) = S.polygon ! A.points "403,360 404,371 394,376 395,382 401,385 406,396 401,402 387,402 367,406 365,412 367,421 370,425 375,423 382,427 390,425 398,427 404,422 410,420 422,420 430,423 432,409 439,404 438,397 427,399 422,382 423,376 414,372 411,361"
provinceElement (Normal Serbia) = S.polygon ! A.points "365,412 360,413 342,410 338,412 335,410 332,410 330,416 331,424 327,429 330,437 337,446 346,452 346,466 350,471 356,471 361,467 369,464 365,461 371,456 366,439 371,438 368,433 365,425 367,421"
provinceElement (Normal Sevastopol) = S.polygon ! A.points "438,397 446,378 459,375 461,377 459,379 465,383 476,381 478,383 472,385 468,392 477,396 477,401 486,404 488,397 494,396 497,392 507,389 506,384 494,387 485,378 503,364 526,351 527,354 514,365 517,371 520,371 515,384 511,383 510,386 517,393 528,394 554,406 567,408 573,417 570,427 589,442 594,439 603,441 609,440 609,330 597,330 569,321 564,305 554,304 549,284 533,283 526,287 516,286 505,280 494,295 477,289 468,295 470,303 466,307 460,345 445,350 434,360 432,372 423,376 422,382 427,399"
provinceElement (Normal Silesia) = S.polygon ! A.points "288,321 297,322 311,334 314,332 321,339 325,340 329,338 333,330 326,327 323,322 320,303 296,300 288,305 284,314"
provinceElement (Normal Skagerrak) = S.polygon ! A.points "241,209 246,210 266,201 270,193 275,203 277,218 276,224 282,236 279,240 279,243 275,242 269,243 266,240 267,234 266,221 263,223 248,224 241,224"
provinceElement (Normal Smyrna) = S.polygon ! A.points "417,489 420,495 417,498 417,507 423,510 427,524 435,523 435,530 441,526 447,528 453,534 464,531 466,521 475,520 485,528 491,530 505,526 511,514 520,517 527,508 530,509 536,494 545,486 555,484 563,479 562,471 556,467 555,460 546,462 531,460 508,480 501,482 490,480 473,491 466,491 452,495 432,493 423,487"
provinceElement (Special SpainNorth) = S.polyline ! A.points "134,417 123,412 113,407 112,399 101,396 96,397 72,384 59,381 54,375 48,374 46,378 39,375 33,381 35,384 32,396 43,395 42,399 55,400 62,407 61,411 52,412 42,432 37,431 40,441"
provinceElement (Special SpainSouth) = S.polyline ! A.points "40,441 34,447 36,457 27,468 33,475 34,484 37,490 47,488 52,489 60,486 78,491 83,494 86,485 90,483 98,484 107,474 113,473 115,469 110,461 124,444 131,439 146,438 157,432 158,425 154,427 142,417 135,414 134,417 123,412"
provinceElement (Special StPetersburgNorth) = S.polyline ! A.points "534,164 564,159 573,143 598,132 609,117 609,0 540 0 535,9 530,6 517,19 516,33 513,38 513,23 507,20 505,26 499,33 492,48 495,58 488,60 479,57 477,55 481,50 473,43 466,45 472,62 478,66 478,74 472,72 468,74 457,91 469,100 467,106 462,109 444,101 442,110 447,115 454,119 452,122 434,118 426,103 426,94 414,88 412,83 445,84 457,79 459,66 453,61 417,47 405,49 401,45 397,48 388,61 387,68 393,73 392,92 401,110 402,118 410,130 414,147"
provinceElement (Special StPetersburgWest) = S.polyline ! A.points "414,147 410,152 412,161 402,177 403,183 411,184 414,187 408,187 400,192 399,197 387,196 371,198 369,202 372,205 382,206 394,205 405,217 409,228 421,229 428,225 439,211 447,209 451,213 457,210 456,207 458,194 476,183 489,184 515,169 534,164 564,159"
provinceElement (Normal Sweden) = S.polygon ! A.points "275,203 277,218 276,224 282,236 279,240 279,243 282,253 289,254 294,245 305,244 312,229 311,220 314,209 322,206 328,203 331,193 326,183 320,182 321,161 330,146 343,138 351,128 347,121 349,112 355,104 362,107 356,71 342,61 341,65 330,64 332,74 324,71 311,101 308,104 309,115 300,126 301,132 292,133 290,164 285,170 287,177 279,204"
provinceElement (Normal Syria) = S.polygon ! A.points "530,509 536,494 545,486 555,484 563,479 584,478 609,493 609,559 528,559 532,535 526,530 525,518"
provinceElement (Normal Trieste) = S.polygon ! A.points "276,399 275,403 278,410 282,401 286,402 289,418 306,436 331,454 330,445 337,446 330,437 327,429 331,424 330,416 332,410 323,408 321,398 311,394 308,383 299,385 294,380 289,385 276,386 279,389"
provinceElement (Normal Tunis) = S.polygon ! A.points "232,559 234,551 232,544 225,535 231,531 236,524 233,523 224,527 223,518 218,516 212,517 208,521 203,520 197,527 195,559"
provinceElement (Normal Tuscany) = S.polygon ! A.points "233,415 238,431 247,442 250,438 263,434 253,418 246,416 240,415 236,411"
provinceElement (Normal Tyrolia) = S.polygon ! A.points "234,366 243,370 246,369 250,371 267,368 271,370 269,362 275,362 281,356 292,357 295,362 294,380 289,385 276,386 268,385 259,388 255,394 250,397 246,392 243,388 245,384 241,378 234,374"
provinceElement (Normal TyrrhenianSea) = S.polygon ! A.points "238,431 247,442 248,447 256,458 271,464 276,474 290,487 294,502 289,511 285,511 285,508 276,510 263,510 257,507 252,508 247,513 236,524 233,523 224,527 223,518 218,516 218,490 220,490 224,468 222,458 218,458 218,454 223,450 225,444 225,436 224,431"
provinceElement (Normal Ukraine) = S.polygon ! A.points "383,327 385,332 399,338 404,354 403,360 411,361 414,372 423,376 432,372 434,360 445,350 460,345 466,307 470,303 468,295 456,292 390,306 386,309"
provinceElement (Normal Venice) = S.polygon ! A.points "278,443 272,424 260,417 261,401 270,398 276,399 279,389 276,386 268,385 259,388 255,394 250,397 246,392 233,404 236,411 240,415 246,416 253,418 263,434 274,447"
provinceElement (Normal Vienna) = S.polygon ! A.points "292,357 295,349 303,346 316,348 322,347 329,346 337,350 335,354 322,370 311,375 308,383 299,385 294,380 295,362"
provinceElement (Normal Wales) = S.polygon ! A.points "100,291 112,287 122,281 130,282 127,276 119,272 116,272 115,265 128,262 143,262 150,264 153,271 150,277 145,281 147,295 134,294 124,291 120,295 110,292"
provinceElement (Normal Warsaw) = S.polygon ! A.points "333,330 326,327 323,322 320,303 324,299 326,292 341,287 345,289 359,286 365,281 372,283 379,290 386,309 383,327 379,324 374,327 367,329 361,324 356,323 353,327 344,332 341,330"
provinceElement (Normal WesternMediterranean) = S.polygon ! A.points "37,490 47,488 52,489 60,486 78,491 83,494 86,485 90,483 98,484 107,474 113,473 115,469 142,469 150,471 154,466 205,466 206,476 204,485 208,492 212,492 217,489 218,490 218,516 212,517 208,521 203,520 179,515 169,518 150,511 117,509 106,511 99,515 89,512 84,518 79,520 68,516 68,511 64,514 46,509 42,502 41,494 37,495"
provinceElement (Normal Yorkshire) = S.polygon ! A.points "163,226 163,239 168,246 170,252 169,265 166,269 153,271 150,264 151,248 155,239 155,228"
provinceElement x = error "provinceElement must only be applied to output of properProvinceTargets"

provinceExtras (Normal Constantinople) = S.polygon ! A.points "414,475 421,467 435,463 440,458 442,460 439,463 448,464 425,475" ! A.style "fill:#99CCFF; stroke:black;"
provinceExtras (Normal Denmark) = S.polygon ! A.points "269,243 268,246 263,247 266,255 254,255 257,247 266,240" ! A.style "fill:#99CCFF; stroke:black;"
provinceExtras _ = return ()

main = putStrLn $ svgMap (initialBoard Seven)
