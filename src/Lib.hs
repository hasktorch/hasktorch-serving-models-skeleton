{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Lib where

import Torch hiding (trace)
import qualified Torch.Functional.Internal as I
import Debug.Trace

bboxIou ::
  Tensor ->
  Tensor ->
  Tensor
bboxIou box1 box2 =
  let b1_x1 = box1 ! [slice|...,0|]
      b1_y1 = box1 ! [slice|...,1|]
      b1_x2 = box1 ! [slice|...,2|]
      b1_y2 = box1 ! [slice|...,3|]
      b2_x1 = box2 ! [slice|...,0|]
      b2_y1 = box2 ! [slice|...,1|]
      b2_x2 = box2 ! [slice|...,2|]
      b2_y2 = box2 ! [slice|...,3|]
      inter_rect_x1 = I.max b1_x1 b2_x1
      inter_rect_y1 = I.max b1_y1 b2_y1
      inter_rect_x2 = I.min b1_x2 b2_x2
      inter_rect_y2 = I.min b1_y2 b2_y2
      inter_area = clampMin 0 (inter_rect_x2 - inter_rect_x1 + 1) * clampMin 0 (inter_rect_y2 - inter_rect_y1 + 1)
      b1_area = (b1_x2 - b1_x1 + 1) * (b1_y2 - b1_y1 + 1)
      b2_area = (b2_x2 - b2_x1 + 1) * (b2_y2 - b2_y1 + 1)
   in inter_area / (b1_area + b2_area - inter_area + 1e-16)

-- | Returns xyxy format from xywh
xywh2xyxy ::
  -- | input (batch,grid,4)
  Tensor ->
  -- | output (batch,grid,4)
  Tensor
xywh2xyxy xywh =
  let x = xywh ! [slice|...,0|]
      y = xywh ! [slice|...,1|]
      w = xywh ! [slice|...,2|]
      h = xywh ! [slice|...,3|]
      other = xywh ! [slice|...,4:|]
   in cat
        (Dim (-1))
        [ stack
            (Dim (-1))
            [ x - ((0.5::Float) `mulScalar` w),
              y - ((0.5::Float) `mulScalar` h),
              x + ((0.5::Float) `mulScalar` w),
              y + ((0.5::Float) `mulScalar` h)
            ],
          other
        ]

-- | Returns objects that exceed a threshold.
toDetection ::
  -- | input
  Tensor ->
  -- | confidence threshold
  Float ->
  -- | (the number of objects that exceed the threshold)
  Maybe Tensor
toDetection prediction conf_thres =
  if head (shape prediction) == 0
    then Nothing
    else
      if head (shape prediction') >= 1
        then 
          let (values, indices) = trace (show (shape prediction')) maxDim (Dim (-1)) RemoveDim $ prediction' ! [slice|...,5:|]
              list_of_detections =
                [ prediction' ! [slice|...,0:5|],
                  stack
                    (Dim (-1))
                    [ values,
                      indices,
                      ids
                    ]
                ]
              detections = cat (Dim (-1)) list_of_detections
              score = prediction' ! [slice|...,4|] * values
              detections' = detections ! I.argsort score (-1) True
           in Just detections'
         else
           Nothing
  where
    i0 = trace (show $ prediction ! [slice|0,0,4|]) prediction
    (v_i0,i_i0) = maxDim (Dim (-1)) RemoveDim $ i0 ! [slice|...,5:|]
    p1 = ((i0 ! [slice|...,4|]) `ge` asTensor conf_thres)
    p2 = ((v_i0 ! [slice|...,4|]) `ge` asTensor conf_thres)
    indexes = p1 -- `I.logical_and` p2
    prediction' = xywh2xyxy $ i0 ! indexes
    n = reverse (shape prediction) !! 1
    ids = reshape [1, n] (arange' (0 :: Int) n (1 :: Int)) ! indexes

nonMaxSuppression ::
  Tensor ->
  Float ->
  Float ->
  [Tensor]
nonMaxSuppression prediction conf_thres nms_thres =
  maybe [] loop (toDetection prediction conf_thres)
  where
    loop :: Tensor -> [Tensor]
    loop detections =
      if size 0 detections == 0
        then []
        else
          let detection0 = detections ! (0::Int)
              large_overlap =
                bboxIou
                  (unsqueeze (Dim 0) (detection0 ! [slice|:4|]))
                  (detections ! [slice|...,:4|])
                  `ge` asTensor nms_thres
              label_match = detection0 ! 6 `eq` detections ! [slice|...,6|]
              invalid = large_overlap `I.logical_and` label_match
              weights = unsqueeze (Dim 1) $ detections ! [slice|{invalid},4|]
              detections' = detections ! I.logical_not invalid
              detection' = sumDim (Dim 0) RemoveDim (dtype prediction) (weights * (detections ! (invalid, [slice|0:4|]))) / sumAll weights
           in cat (Dim (-1)) [detection', detection0 ! [slice|4:|]] : loop detections'

