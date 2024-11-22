module Main where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Text qualified as T
import Diagrams qualified as D
import Diagrams.Backend.SVG qualified as SVG
import Diagrams.Backend.SVG.CmdLine qualified as D
import Diagrams.Prelude qualified as D
import Text.Read qualified as String

readMaybe :: (Read a) => T.Text -> Maybe a
readMaybe = T.unpack >>> String.readMaybe

parseStroke :: forall m n. (MonadFail m, Num n, Read n) => T.Text -> m (D.V2 n, [D.Segment D.Closed D.V2 n])
parseStroke =
  T.words >>> \case
    "M" : (readMaybe -> Just x) : (readMaybe -> Just y) : rest ->
      let start = D.V2 x y
       in (start,) <$> parseTail start rest
    ws -> "Failed to parse stroke: " <> (take 10 ws & show) & fail
  where
    parseTail :: D.V2 n -> [T.Text] -> m [D.Segment D.Closed D.V2 n]
    parseTail prev = \case
      [] -> pure []
      ["Z"] -> pure []
      "L"
        : (readMaybe -> Just x)
        : (readMaybe -> Just y)
        : rest ->
          let this = D.V2 x y
           in ((this - prev & D.OffsetClosed & D.Linear) :) <$> parseTail this rest
      "Q"
        : (readMaybe @n -> Just x1)
        : (readMaybe @n -> Just y1)
        : (readMaybe -> Just x)
        : (readMaybe -> Just y)
        : rest ->
          let this = D.V2 x y
              cp = D.V2 x1 y1 - prev
           in ((this - prev & D.OffsetClosed & D.Cubic 0 cp) :) <$> parseTail this rest
      "C"
        : (readMaybe @n -> Just _)
        : (readMaybe @n -> Just _)
        : (readMaybe @n -> Just _)
        : (readMaybe @n -> Just _)
        : (readMaybe @n -> Just x)
        : (readMaybe @n -> Just y)
        : rest ->
          let this = D.V2 x y
           in ((this - prev & D.OffsetClosed & D.Linear) :) <$> parseTail this rest
      ws -> "Failed to parse segments: " <> (take 10 ws & show) & fail

好strokesRaw :: [T.Text]
好strokesRaw =
  [ "M 330 202 Q 361 175 399 134 Q 415 119 424 118 Q 433 118 439 128 Q 446 138 442 170 Q 435 206 361 247 L 319 270 Q 292 286 258 304 Q 237 314 240 335 Q 261 393 281 453 L 293 492 Q 317 568 337 644 Q 347 690 366 715 Q 379 737 373 750 Q 360 769 313 797 Q 294 810 276 801 Q 263 794 273 778 Q 303 733 247 486 L 236 442 Q 218 373 195 336 Q 185 314 206 296 Q 254 268 294 233 L 330 202 Z",
    "M 294 233 Q 287 226 281 217 Q 250 180 196 143 Q 183 134 165 124 Q 149 114 133 104 Q 120 95 131 92 Q 212 86 327 199 Q 328 200 330 202 L 361 247 Q 406 322 421 385 Q 449 488 463 510 Q 473 526 458 537 Q 416 576 387 569 Q 374 565 378 550 Q 387 531 387 507 L 385 481 Q 384 469 382 455 Q 375 376 319 270 L 294 233 Z",
    "M 387 507 Q 341 501 293 492 L 247 486 Q 183 479 115 468 Q 94 465 61 471 Q 48 471 45 462 Q 41 450 49 441 Q 68 422 96 400 Q 106 396 118 402 Q 190 436 236 442 L 281 453 Q 320 463 362 474 Q 372 478 385 481 C 414 489 417 511 387 507 Z",
    "M 671 521 Q 788 635 822 648 Q 843 655 835 672 Q 831 688 760 725 Q 739 735 716 725 Q 661 703 575 676 Q 553 669 498 669 Q 473 669 482 648 Q 491 635 511 623 Q 544 605 578 627 Q 597 636 691 676 Q 706 682 719 673 Q 732 664 726 649 Q 693 595 655 531 C 640 505 649 500 671 521 Z",
    "M 717 430 Q 702 497 671 521 L 655 531 Q 648 535 640 538 Q 618 547 608 540 Q 595 533 608 519 Q 645 491 653 444 Q 656 434 659 421 L 668 384 Q 701 204 658 103 Q 643 76 607 83 Q 576 89 548 94 Q 536 97 542 85 Q 546 78 564 65 Q 604 31 618 5 Q 628 -14 645 -11 Q 660 -10 687 17 Q 775 107 726 391 L 717 430 Z",
    "M 726 391 Q 783 397 947 397 Q 966 398 971 406 Q 977 416 960 430 Q 909 467 848 454 Q 793 445 717 430 L 659 421 Q 562 409 452 393 Q 431 392 447 375 Q 460 362 478 357 Q 497 351 514 356 Q 586 375 668 384 L 726 391 Z"
  ]

main :: IO ()
main = do
  strokes <-
    traverse
      (parseStroke @IO @Double)
      好strokesRaw

  let diag :: D.Diagram SVG.SVG =
        foldMap
          ( \(start, segs) ->
              D.trailFromSegments segs
                & D.closeTrail
                & D.strokeTrail
                & D.translate start
                & D.fc D.black
          )
          strokes

  D.mainWith diag
