(ns portkey.aws.kinesisvideo (:require [portkey.aws]))

(def endpoints 'nil)

(clojure.spec.alpha/def :portkey.aws.kinesisvideo/fragment-number-string (clojure.spec.alpha/and clojure.core/string? (clojure.core/fn [s__15966__auto__] (clojure.core/<= 1 (clojure.core/count s__15966__auto__))) (clojure.core/fn [s__15967__auto__] (clojure.core/< (clojure.core/count s__15967__auto__) 128)) (clojure.core/fn [s__15968__auto__] (clojure.core/re-matches #"^[0-9]+$" s__15968__auto__))))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo/stream-name (clojure.spec.alpha/and clojure.core/string? (clojure.core/fn [s__15966__auto__] (clojure.core/<= 1 (clojure.core/count s__15966__auto__))) (clojure.core/fn [s__15967__auto__] (clojure.core/< (clojure.core/count s__15967__auto__) 256)) (clojure.core/fn [s__15968__auto__] (clojure.core/re-matches #"[a-zA-Z0-9_.-]+" s__15968__auto__))))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo.get-media-input/streamarn (clojure.spec.alpha/and :portkey.aws.kinesisvideo/resourcearn))
(clojure.spec.alpha/def :portkey.aws.kinesisvideo/get-media-input (portkey.aws/json-keys :req-un [:portkey.aws.kinesisvideo/StartSelector] :opt-un [:portkey.aws.kinesisvideo/StreamName :portkey.aws.kinesisvideo.get-media-input/StreamARN] :locations {}))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo.invalid-endpoint-exception/message (clojure.spec.alpha/and :portkey.aws.kinesisvideo/error-message))
(clojure.spec.alpha/def :portkey.aws.kinesisvideo/invalid-endpoint-exception (portkey.aws/json-keys :req-un [] :opt-un [:portkey.aws.kinesisvideo.invalid-endpoint-exception/Message] :locations {}))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo.not-authorized-exception/message (clojure.spec.alpha/and :portkey.aws.kinesisvideo/error-message))
(clojure.spec.alpha/def :portkey.aws.kinesisvideo/not-authorized-exception (portkey.aws/json-keys :req-un [] :opt-un [:portkey.aws.kinesisvideo.not-authorized-exception/Message] :locations {}))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo/payload (clojure.spec.alpha/and clojure.core/bytes? (clojure.spec.alpha/conformer portkey.aws/base64-encode portkey.aws/base64-decode)))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo/error-message (clojure.spec.alpha/and clojure.core/string?))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo/content-type (clojure.spec.alpha/and clojure.core/string? (clojure.core/fn [s__15966__auto__] (clojure.core/<= 1 (clojure.core/count s__15966__auto__))) (clojure.core/fn [s__15967__auto__] (clojure.core/< (clojure.core/count s__15967__auto__) 128)) (clojure.core/fn [s__15968__auto__] (clojure.core/re-matches #"^[a-zA-Z0-9_\.\-]+$" s__15968__auto__))))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo.start-selector/after-fragment-number (clojure.spec.alpha/and :portkey.aws.kinesisvideo/fragment-number-string))
(clojure.spec.alpha/def :portkey.aws.kinesisvideo.start-selector/start-timestamp (clojure.spec.alpha/and :portkey.aws.kinesisvideo/timestamp))
(clojure.spec.alpha/def :portkey.aws.kinesisvideo/start-selector (portkey.aws/json-keys :req-un [:portkey.aws.kinesisvideo/StartSelectorType] :opt-un [:portkey.aws.kinesisvideo.start-selector/AfterFragmentNumber :portkey.aws.kinesisvideo.start-selector/StartTimestamp :portkey.aws.kinesisvideo/ContinuationToken] :locations {}))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo.resource-not-found-exception/message (clojure.spec.alpha/and :portkey.aws.kinesisvideo/error-message))
(clojure.spec.alpha/def :portkey.aws.kinesisvideo/resource-not-found-exception (portkey.aws/json-keys :req-un [] :opt-un [:portkey.aws.kinesisvideo.resource-not-found-exception/Message] :locations {}))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo/start-selector-type (clojure.spec.alpha/conformer (clojure.core/let [m__15964__auto__ {"EARLIEST" "EARLIEST", "NOW" "NOW", "FRAGMENT_NUMBER" "FRAGMENT_NUMBER", "PRODUCER_TIMESTAMP" "PRODUCER_TIMESTAMP", :now "NOW", :producer-timestamp "PRODUCER_TIMESTAMP", :fragment-number "FRAGMENT_NUMBER", :earliest "EARLIEST", :continuation-token "CONTINUATION_TOKEN", "SERVER_TIMESTAMP" "SERVER_TIMESTAMP", "CONTINUATION_TOKEN" "CONTINUATION_TOKEN", :server-timestamp "SERVER_TIMESTAMP"}] (clojure.core/fn [s__15965__auto__] (m__15964__auto__ s__15965__auto__ :clojure.spec.alpha/invalid))) (clojure.core/comp clojure.core/keyword portkey.aws/dashed)))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo.put-media-input/streamarn (clojure.spec.alpha/and :portkey.aws.kinesisvideo/resourcearn))
(clojure.spec.alpha/def :portkey.aws.kinesisvideo.put-media-input/producer-start-timestamp (clojure.spec.alpha/and :portkey.aws.kinesisvideo/timestamp))
(clojure.spec.alpha/def :portkey.aws.kinesisvideo/put-media-input (portkey.aws/json-keys :req-un [:portkey.aws.kinesisvideo/FragmentTimecodeType] :opt-un [:portkey.aws.kinesisvideo/StreamName :portkey.aws.kinesisvideo.put-media-input/StreamARN :portkey.aws.kinesisvideo.put-media-input/ProducerStartTimestamp :portkey.aws.kinesisvideo/Payload] :locations {}))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo/put-media-output (portkey.aws/json-keys :req-un [] :opt-un [:portkey.aws.kinesisvideo/Payload] :locations {}))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo.client-limit-exceeded-exception/message (clojure.spec.alpha/and :portkey.aws.kinesisvideo/error-message))
(clojure.spec.alpha/def :portkey.aws.kinesisvideo/client-limit-exceeded-exception (portkey.aws/json-keys :req-un [] :opt-un [:portkey.aws.kinesisvideo.client-limit-exceeded-exception/Message] :locations {}))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo/continuation-token (clojure.spec.alpha/and clojure.core/string? (clojure.core/fn [s__15966__auto__] (clojure.core/<= 1 (clojure.core/count s__15966__auto__))) (clojure.core/fn [s__15967__auto__] (clojure.core/< (clojure.core/count s__15967__auto__) 128)) (clojure.core/fn [s__15968__auto__] (clojure.core/re-matches #"^[a-zA-Z0-9_\.\-]+$" s__15968__auto__))))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo.invalid-argument-exception/message (clojure.spec.alpha/and :portkey.aws.kinesisvideo/error-message))
(clojure.spec.alpha/def :portkey.aws.kinesisvideo/invalid-argument-exception (portkey.aws/json-keys :req-un [] :opt-un [:portkey.aws.kinesisvideo.invalid-argument-exception/Message] :locations {}))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo/timestamp clojure.core/inst?)

(clojure.spec.alpha/def :portkey.aws.kinesisvideo.connection-limit-exceeded-exception/message (clojure.spec.alpha/and :portkey.aws.kinesisvideo/error-message))
(clojure.spec.alpha/def :portkey.aws.kinesisvideo/connection-limit-exceeded-exception (portkey.aws/json-keys :req-un [] :opt-un [:portkey.aws.kinesisvideo.connection-limit-exceeded-exception/Message] :locations {}))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo/resourcearn (clojure.spec.alpha/and clojure.core/string? (clojure.core/fn [s__15966__auto__] (clojure.core/<= 1 (clojure.core/count s__15966__auto__))) (clojure.core/fn [s__15967__auto__] (clojure.core/< (clojure.core/count s__15967__auto__) 1024)) (clojure.core/fn [s__15968__auto__] (clojure.core/re-matches #"arn:aws:kinesisvideo:[a-z0-9-]+:[0-9]+:[a-z]+/[a-zA-Z0-9_.-]+/[0-9]+" s__15968__auto__))))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo/get-media-output (portkey.aws/json-keys :req-un [] :opt-un [:portkey.aws.kinesisvideo/ContentType :portkey.aws.kinesisvideo/Payload] :locations {}))

(clojure.spec.alpha/def :portkey.aws.kinesisvideo/fragment-timecode-type (clojure.spec.alpha/conformer (clojure.core/let [m__15964__auto__ {"ABSOLUTE" "ABSOLUTE", :absolute "ABSOLUTE", "RELATIVE" "RELATIVE", :relative "RELATIVE"}] (clojure.core/fn [s__15965__auto__] (m__15964__auto__ s__15965__auto__ :clojure.spec.alpha/invalid))) (clojure.core/comp clojure.core/keyword portkey.aws/dashed)))

(clojure.core/defn get-media " Use this API to retrieve media content from a Kinesis video stream. In the request, you identify stream name or stream Amazon Resource Name (ARN), and the starting chunk. Kinesis Video Streams then returns a stream of chunks in order by fragment number.\n   You must first call the GetDataEndpoint API to get an endpoint to which you can then send the GetMedia requests. \n  When you put media data (fragments) on a stream, Kinesis Video Streams stores each incoming fragment and related metadata in what is called a \"chunk.\" For more information, see . The GetMedia API returns a stream of these chunks starting from the chunk that you specify in the request. \n The following limits apply when using the GetMedia API:\n  \n * A client can call GetMedia up to five times per second per stream. \n  \n * Kinesis Video Streams sends media data at a rate of up to 25 megabytes per second (or 200 megabits per second) during a GetMedia session. \n  " ([get-media-input] (portkey.aws/-rest-json-call portkey.aws.kinesisvideo/endpoints "POST" "/getMedia" get-media-input :portkey.aws.kinesisvideo/get-media-input {:payload nil, :move {}, :headers {}, :uri {}, :querystring {}} nil :portkey.aws.kinesisvideo/get-media-output {"ResourceNotFoundException" :portkey.aws.kinesisvideo/resource-not-found-exception, "NotAuthorizedException" :portkey.aws.kinesisvideo/not-authorized-exception, "InvalidEndpointException" :portkey.aws.kinesisvideo/invalid-endpoint-exception, "ClientLimitExceededException" :portkey.aws.kinesisvideo/client-limit-exceeded-exception, "ConnectionLimitExceededException" :portkey.aws.kinesisvideo/connection-limit-exceeded-exception, "InvalidArgumentException" :portkey.aws.kinesisvideo/invalid-argument-exception})))
(clojure.spec.alpha/fdef get-media :args (clojure.spec.alpha/tuple :portkey.aws.kinesisvideo/get-media-input) :ret (clojure.spec.alpha/and :portkey.aws.kinesisvideo/get-media-output))

(clojure.core/defn put-media " Use this API to send media data to a Kinesis video stream. \n   Before using this API, you must call the GetDataEndpoint API to get an endpoint. You then specify the endpoint in your PutMedia request. \n   In the request, you use the HTTP headers to provide parameter information, for example, stream name, time stamp, and whether the time stamp value is absolute or relative to when the producer started recording. You use the request body to send the media data. Kinesis Video Streams supports only the Matroska (MKV) container format for sending media data using this API. \n You have the following options for sending data using this API:\n  \n * Send media data in real time: For example, a security camera can send frames in real time as it generates them. This approach minimizes the latency between the video recording and data sent on the wire. This is referred to as a continuous producer. In this case, a consumer application can read the stream in real time or when needed. \n  \n * Send media data offline (in batches): For example, a body camera might record video for hours and store it on the device. Later, when you connect the camera to the docking port, the camera can start a PutMedia session to send data to a Kinesis video stream. In this scenario, latency is not an issue. \n   When using this API, note the following considerations:\n  \n * You must specify either streamName or streamARN, but not both.\n  \n *  You might find it easier to use a single long-running PutMedia session and send a large number of media data fragments in the payload. Note that for each fragment received, Kinesis Video Streams sends one or more acknowledgements. Potential network considerations might cause you to not get all these acknowledgements as they are generated. \n  \n *  You might choose multiple consecutive PutMedia sessions, each with fewer fragments to ensure that you get all acknowledgements from the service in real time. \n    If you send data to the same stream on multiple simultaneous PutMedia sessions, the media fragments get interleaved on the stream. You should make sure that this is OK in your application scenario. \n  The following limits apply when using the PutMedia API:\n  \n * A client can call PutMedia up to five times per second per stream.\n  \n * A client can send up to five fragments per second per stream.\n  \n * Kinesis Video Streams reads media data at a rate of up to 12.5 MB/second, or 100 Mbps during a PutMedia session. \n   Note the following constraints. In these cases, Kinesis Video Streams sends the Error acknowledgement in the response. \n  \n * Fragments that have time codes spanning longer than 10 seconds and that contain more than 50 megabytes of data are not allowed. \n  \n *  An MKV stream containing more than one MKV segment or containing disallowed MKV elements (like track*) also results in the Error acknowledgement. \n   Kinesis Video Streams stores each incoming fragment and related metadata in what is called a \"chunk.\" The fragment metadata includes the following: \n  \n * The MKV headers provided at the start of the PutMedia request\n  \n * The following Kinesis Video Streams-specific metadata for the fragment:\n  \n *  server_timestamp - Time stamp when Kinesis Video Streams started receiving the fragment. \n  \n *  producer_timestamp - Time stamp, when the producer started recording the fragment. Kinesis Video Streams uses three pieces of information received in the request to calculate this value. \n  \n * The fragment timecode value received in the request body along with the fragment.\n  \n * Two request headers: producerStartTimestamp (when the producer started recording) and fragmentTimeCodeType (whether the fragment timecode in the payload is absolute or relative).\n   Kinesis Video Streams then computes the producer_timestamp for the fragment as follows:\n  If fragmentTimeCodeType is relative, then \n  producer_timestamp = producerStartTimeSamp + fragment timecode \n If fragmentTimeCodeType is absolute, then \n  producer_timestamp = fragment timecode (converted to milliseconds)\n  \n * Unique fragment number assigned by Kinesis Video Streams.\n   \n     When you make the GetMedia request, Kinesis Video Streams returns a stream of these chunks. The client can process the metadata as needed. \n   This operation is only available for the AWS SDK for Java. It is not supported in AWS SDKs for other languages.\n " ([put-media-input] (portkey.aws/-rest-json-call portkey.aws.kinesisvideo/endpoints "POST" "/putMedia" put-media-input :portkey.aws.kinesisvideo/put-media-input {:payload "Payload", :move {}, :headers {"StreamName" ["x-amzn-stream-name" nil], "StreamARN" ["x-amzn-stream-arn" nil], "FragmentTimecodeType" ["x-amzn-fragment-timecode-type" nil], "ProducerStartTimestamp" ["x-amzn-producer-start-timestamp" nil]}, :uri {}, :querystring {}} nil :portkey.aws.kinesisvideo/put-media-output {"ResourceNotFoundException" :portkey.aws.kinesisvideo/resource-not-found-exception, "NotAuthorizedException" :portkey.aws.kinesisvideo/not-authorized-exception, "InvalidEndpointException" :portkey.aws.kinesisvideo/invalid-endpoint-exception, "InvalidArgumentException" :portkey.aws.kinesisvideo/invalid-argument-exception, "ClientLimitExceededException" :portkey.aws.kinesisvideo/client-limit-exceeded-exception, "ConnectionLimitExceededException" :portkey.aws.kinesisvideo/connection-limit-exceeded-exception})))
(clojure.spec.alpha/fdef put-media :args (clojure.spec.alpha/tuple :portkey.aws.kinesisvideo/put-media-input) :ret (clojure.spec.alpha/and :portkey.aws.kinesisvideo/put-media-output))
