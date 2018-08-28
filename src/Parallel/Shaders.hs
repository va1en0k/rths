{-# LANGUAGE QuasiQuotes #-}

module Parallel.Shaders where

import           Control.Parallel.CLUtil        ( OpenCLState(OpenCLState)
                                                , bufferToVector
                                                , clContext
                                                , clDevice
                                                , clQueue
                                                , writeVectorToBuffer
                                                )
import           Control.Parallel.OpenCL

import           Control.Monad                  ( forM_ )
import           Data.Vector.Storable           ( Vector )
import qualified Data.Vector.Storable          as V
import           Foreign                        ( nullPtr
                                                , sizeOf
                                                )
import           Foreign.C.Types                ( CFloat )
import           Language.C.Quote.OpenCL        ( cfun )
-- import           Text.PrettyPrint.Mainland      ( prettyCompact )
-- import           Text.PrettyPrint.Mainland.Class
--                                                 ( ppr )


data ShaderEngine = ShaderEngine { openCLState :: OpenCLState }
data Shader = Shader CLKernel

initShaderEngine :: IO ShaderEngine
initShaderEngine = do
  context <- clCreateContextFromType [] [CL_DEVICE_TYPE_DEFAULT] print
  device  <- last <$> clGetContextDevices context
  -- pname device
  dname device
  queue <- clCreateCommandQueue context device []

  return $ ShaderEngine $ OpenCLState
    { clDevice  = device
    , clContext = context
    , clQueue   = queue
    }

createShader :: ShaderEngine -> String -> IO Shader
createShader engine source = do
  let OpenCLState { clDevice = device, clContext = context, clQueue = queue } =
        openCLState engine
  program <- clCreateProgramWithSource context source
  clBuildProgram program [device] ""
  kernel <- clCreateKernel program "doubleArray"
  return $ Shader kernel

  --

runOnShader
  :: ShaderEngine -> Shader -> Int -> Int -> [[[Double]]] -> IO [Double]
runOnShader engine shader outcount outsize inputs = do
  let OpenCLState { clDevice = device, clContext = context, clQueue = queue } =
        openCLState engine
  let (Shader kernel) = shader
  -- Set up memory
  let dimensions      = map length inputs
  let outSize         = outcount * outsize
  let nOutBytes = outSize * sizeOf (undefined :: CFloat)

  -- Buffers for input and output data.
  -- We request OpenCL to create a buffer on the host (CL_MEM_ALLOC_HOST_PTR)
  -- since we're using CPU. The performance here may not be ideal, because
  -- we're copying the buffer. However, it's safe, and not unduly nested.

  bufOut <- clCreateBuffer context
                           [CL_MEM_WRITE_ONLY, CL_MEM_ALLOC_HOST_PTR]
                           (nOutBytes, nullPtr)

  -- Copy our input data Vector to the input buffer; blocks until complete


  -- Run the kernel
  mapM
    (\(i, input) -> do
      let inputData :: Vector CFloat
          inputData = V.fromList $ map realToFrac $ concat input

          nElem     = V.length inputData
          nInBytes  = nElem * sizeOf (undefined :: CFloat)
      print (i, nElem, nInBytes)
      bufIn <- clCreateBuffer context
                              [CL_MEM_READ_ONLY, CL_MEM_ALLOC_HOST_PTR]
                              (nInBytes, nullPtr)
      writeVectorToBuffer (openCLState engine) bufIn inputData
      clSetKernelArgSto kernel i bufIn
    )
    (zip [0 ..] inputs)
  clSetKernelArgSto kernel (fromIntegral $ length inputs) bufOut
  execEvent <- clEnqueueNDRangeKernel queue
                                      kernel
                                      [(length $ head inputs), 1]
                                      []
                                      []

  -- Get the result; blocks until complete
  outputData <-
    bufferToVector queue bufOut outSize [execEvent] :: IO (Vector CFloat)

  return $ map realToFrac $ V.toList outputData

-- | The kernel to execute: the equivalient of 'map (*2)'.
-- kernelSource :: String
-- kernelSource = prettyCompact $ ppr $ [cfun|
--     /* This example kernel just does `map (*2)` */
--     kernel void doubleArray(
--         global float *in,
--         global float *out
--     ) {
--         int i = get_global_id(0);
--         out[i] = 2 * in[i];
--     }
-- |]



main :: IO ()
main = do
  putStrLn "* Hello World OpenCL Example *"

  -- Describe the OpenCL Environment
  putStrLn "\n* OpenCL Platform Environment *"
  describePlatforms

  -- Create a Context, Queue and a CLUtil OpenCLState
  engine <- initShaderEngine
  let state = openCLState engine
  let OpenCLState { clDevice = device, clContext = context, clQueue = queue }
        = state

  -- Create the Kernel
  -- shader <- createShader engine kernelSource
  shader <- undefined
  let (Shader kernel) = shader


  let inputData       = [[(-4) .. 4]]
  let outSize         = length inputData

  output <- runOnShader engine shader 1 1 [inputData]



  -- Clean up the Context
  _      <- clReleaseContext context

  -- Show our work
  putStrLn "\n* Results *"
  putStrLn $ "Input:  " ++ show inputData
  putStrLn $ "Output: " ++ show output



-- | Summarises the OpenCL Platforms and their Devices.
--
--   The names of Platforms and all Devices belonging to them are printed to
--   stdout.
describePlatforms :: IO ()
describePlatforms = do

    -- fetch the list of OpenCL Platforms
  platformList <- clGetPlatformIDs :: IO [CLPlatformID]

  -- for each platform,
  forM_ platformList $ \platform -> do

      -- fetch the list of OpenCL Devices
    devs <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL :: IO [CLDeviceID]

    -- print the Platform name and Device names
    pname platform
    forM_ devs dname

putPair name value = putStrLn (name ++ value)
pname p = clGetPlatformInfo p CL_PLATFORM_NAME >>= putPair "Platform: "
dname d = clGetDeviceName d >>= putPair "  Device: "
