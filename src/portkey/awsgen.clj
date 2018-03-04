(ns portkey.awsgen
  (:require [clojure.spec.alpha :as spec]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [net.cgrand.xforms :as x]
            [clojure.string :as str]
            [portkey.aws :as aws]
            [clj-http.client :as http]
            [clojure.string :as str]
            [clojure.pprint]))

#_(def all-apis
    (->> (java.io.File. "resources/aws-sdk-core/apis/")
         file-seq
         (filter #(= "api-2.json" (.getName ^java.io.File %)))
         (map #(with-open [i (io/reader %)] (json/parse-stream i)))))

(defmulti ^:private shape-to-spec (fn [ns [name {:strs [type]}]] type))

(defn- gen-shape-spec [ns [name shape :as e]]
  (let [form (shape-to-spec ns e)]
    (if (and (seq? form) (= 'do (first form)))
      `(~@(butlast form)                                    ; includes do
         (spec/def ~(keyword ns (aws/dashed name)) ~(last form)))
      `(spec/def ~(keyword ns (aws/dashed name)) ~form))))

(defmacro strict-strs [& {:keys [req opt]}]
  `(spec/and
     (spec/every
       (spec/or
         ~@(mapcat (fn [[field spec]]
                     [(keyword field)
                      `(spec/tuple #{~field} ~spec)]) (concat req opt)))
       :kind map?)
     (fn [m#]
       (every? #(contains? m# %) [~@(keys req)]))))

;; :portkey.awsgen/shape is a strict (it fails to validate when unexpected attributes)
;; retro spec 
(defmulti shape-type-spec #(get % "type"))
(spec/def ::shape (spec/multi-spec shape-type-spec #(assoc %1 "type" %2)))

(defmethod shape-to-spec :default [ns [name _ :as kv]]
  (throw (ex-info (str "unsupported shape " name) {:shape kv})))

(defmethod shape-type-spec "structure" [_]
  (strict-strs
    :req {"type"    string?
          "members" (spec/map-of string?
                                 (strict-strs
                                   :req {"shape" string?}
                                   :opt {"location"         #{"uri" "querystring" "header" "headers" "statusCode"}
                                         "locationName"     string?
                                         "queryName"        string?
                                         "deprecated"       boolean?
                                         "idempotencyToken" boolean?
                                         "xmlNamespace"
                                                            (strict-strs :req {"uri" string?}) ; TODO validate
                                         "xmlAttribute"     boolean?
                                         "box"              boolean?
                                         "jsonValue"        boolean?
                                         "jsonvalue"        boolean?
                                         "streaming"        boolean?
                                         "flattened"        boolean?}))}
    :opt {"required"     (spec/coll-of string?)
          "error"        (strict-strs
                           :req {"httpStatusCode" int?}
                           :opt {"code" string? "senderFault" boolean?})
          "exception"    boolean?
          "fault"        boolean?
          "payload"      string?
          "deprecated"   boolean?
          "locationName" string?
          "sensitive"    boolean?
          "wrapper"      boolean?
          "xmlOrder"     (spec/coll-of string?)
          "xmlNamespace"
                         (strict-strs :req {"uri" string?}  ; TODO validate
                                      :opt {"prefix" string?})}))

(defmethod shape-to-spec "structure" [ns [name {:strs [members required payload deprecated error exception]}]]
  (let [spec-names
        (into {} (for [[k {:strs [shape]}] members]
                   [k (keyword (if (not= shape k) (str ns "." (aws/dashed name)) ns) k)]))
        locations
        (into {} (for [[k {:strs [locationName location]}] members
                       :when (and locationName (nil? location))]
                   [locationName k]))]
    `(do
       ~@(for [[k {:strs [shape] :as v}] members
               :when (not= shape k)]
           `(spec/def ~(keyword (str ns "." (aws/dashed name)) (aws/dashed k)) (spec/and ~(keyword ns (aws/dashed shape))))) ; spec/and is a hack to delay resolution
       (aws/json-keys :req-un ~(into [] (map spec-names) required)
                      :opt-un ~(into [] (comp (remove (set required)) (map spec-names))
                                     (keys members))
                      :locations ~locations))))

(defmethod shape-type-spec "list" [_]
  (strict-strs
    :req {"type"   string?
          "member" (strict-strs :req {"shape" string?}
                                :opt {"locationName" string?})}
    :opt {"max"        int?
          "min"        int?
          "deprecated" boolean?
          "flattened"  boolean?
          "sensitive"  boolean?}))

(defmethod shape-to-spec "list" [ns [name {{:strs [shape]} "member" :strs [min max]}]]
  `(spec/and
     (spec/coll-of ~(keyword ns (aws/dashed shape)) ~@(when min `[:min-count ~min]) ~@(when max `[:max-count ~max]))
     (spec/conformer identity #(if (sequential? %) % [%])))) ; HAL ❤️

(defmethod shape-type-spec "boolean" [_]
  (strict-strs :req {"type" string?}
               :opt {"box" boolean?}))

(defmethod shape-to-spec "boolean" [ns [name _]] `boolean?)

(defmethod shape-type-spec "map" [_]
  (strict-strs
    :req {"type"  string?
          "key"   (strict-strs :req {"shape" string?} :opt {"locationName" string?})
          "value" (strict-strs :req {"shape" string?} :opt {"locationName" string?})}
    :opt {"sensitive"    boolean?
          "max"          int?
          "min"          int?
          "flattened"    boolean?
          "locationName" string?}))

(defmethod shape-to-spec "map" [ns [name {:strs [key value sensitive]}]]
  `(spec/map-of ~(keyword ns (aws/dashed (key "shape"))) ~(keyword ns (aws/dashed (value "shape")))))

(defmethod shape-type-spec "string" [_]
  (strict-strs
    :req {"type" string?}
    :opt {"max"       int?
          "min"       int?
          "pattern"   string?
          "enum"      (spec/coll-of string?)
          "sensitive" boolean?}))

(defmethod shape-to-spec "string" [ns [name {:strs [min max sensitive pattern enum]}]]
  (if enum
    `(spec/conformer
       (let [m# ~(into {} (mapcat (fn [s] [[s s] [(keyword (aws/dashed s)) s]])) enum)]
         (fn [s#] (m# s# ::spec/invalid)))
       (comp keyword aws/dashed))
    `(spec/and string?
               ~@(when min [`(fn [s#] (<= ~min (count s#)))])
               ~@(when max [`(fn [s#] (< (count s#) ~max))])
               ~@(when pattern [`(fn [s#] (re-matches ~(re-pattern pattern) s#))]))))

(defmethod shape-type-spec "blob" [_]
  (strict-strs
    :req {"type" string?}
    :opt {"streaming" boolean?
          "max"       int?
          "min"       int?
          "sensitive" boolean?}))

(defmethod shape-to-spec "blob" [ns [name {:strs [streaming sensitive]}]]
  `(spec/and bytes? (spec/conformer aws/base64-encode aws/base64-decode)))

(defmethod shape-type-spec "long" [_]
  (strict-strs
    :req {"type" string?}
    :opt {"max" int?
          "min" int?}))

(defmethod shape-to-spec "long" [ns _] `int?)

(defmethod shape-type-spec "integer" [_]
  (strict-strs
    :req {"type" string?}
    :opt {"max"        int?
          "min"        int?
          "box"        boolean?
          "deprecated" boolean?}))

(defmethod shape-to-spec "integer" [ns [name {:strs [min max]}]]
  `(spec/and int? ~@(when min [`#(<= ~min %)]) ~@(when max [`#(<= % ~max)])))

(defmethod shape-type-spec "timestamp" [_]
  (strict-strs :req {"type" string?}
               :opt {"timestampFormat" #{"iso8601"}}))      ; TODO pattern

(defmethod shape-to-spec "timestamp" [ns _] `inst?)

(defmethod shape-type-spec "double" [_]
  (strict-strs :req {"type" string?}
               :opt {"min" number?
                     "max" number?
                     "box" boolean?}))

(defmethod shape-to-spec "double" [ns _] `double?)

(defmethod shape-type-spec "float" [_]
  (strict-strs :req {"type" string?}
               :opt {"min" number?
                     "max" number?}))

(defmethod shape-to-spec "float" [ns _] `double?)

(spec/def ::operation
  (strict-strs
    :req {"name" string?
          "http" (strict-strs
                   :req {"method"     #{"POST" "DELETE" "GET" "PATCH" "PUT" "HEAD"}
                         "requestUri" string?}
                   :opt {"responseCode" int?})}
    :opt {"input"            (strict-strs :req {"shape" string?}
                                          :opt {"xmlNamespace"
                                                               (strict-strs :req {"uri" string?})
                                                "locationName" string?}) ; TODO validate
          "output"           (strict-strs :req {"shape" string?}
                                          :opt {"resultWrapper" string?})
          "idempotent"       boolean?
          "errors"
                             (spec/coll-of
                               (strict-strs :req {"shape" string?}
                                            :opt {"exception" boolean?
                                                  "fault"     true?
                                                  "error"     (strict-strs
                                                                :req {"httpStatusCode" int?}
                                                                :opt {"code" string?, "senderFault" boolean?})}))
          "documentationUrl" string?                        ; TODO
          "alias"            string?
          "authtype"         #{"none" "v4-unsigned-body"}
          "deprecated"       boolean?}))

#_(str/replace uri #"\{(.*)}" (fn [[_ name]]))

(defn gen-operation [ns {:as                                      operation
                         :strs                                    [name errors]
                         {input-shape "shape"}                    "input"
                         {output-shape "shape"}                   "output"
                         {:strs [method requestUri responseCode]} "http"}
                     shapes]
  (let [error-specs (into {}
                          (map (fn [{:strs [shape] {:strs [httpStatusCode]} "error"}]
                                 [shape (keyword ns (aws/dashed shape))]))
                          errors)
        varname (symbol (aws/dashed name))
        input-spec (some->> input-shape aws/dashed (keyword ns))
        output-spec (some->> output-shape aws/dashed (keyword ns))
        input (or (some-> input-shape aws/dashed symbol) '_)
        default-arg (if input-spec (some #(when (spec/valid? input-spec %) %) [[] {}]) {})]
    (when input-shape
      (aws/conform-or-throw
        (strict-strs                                        ; validate only what we knows how to map
          :req {"type"    #{"structure"}
                "members" (spec/map-of string?
                                       (spec/or
                                         :atomic
                                         (spec/and
                                           (strict-strs
                                             :req {"shape" string?}
                                             :opt {"location"     #{"uri" "querystring" "header" #_#_"headers" "statusCode"}
                                                   "locationName" string?
                                                   "deprecated"   boolean?})
                                           #(= (contains? % "location") (contains? % "locationName")))
                                         :querystringmap
                                         (strict-strs
                                           :req {"shape" string?}
                                           :opt {"location" #{"querystring"}})
                                         :move
                                         (strict-strs
                                           :req {"shape" string?}
                                           :opt {"locationName" string?})
                                         :json-value
                                         (strict-strs
                                           :req {"shape"        string?
                                                 "location"     #{"header"}
                                                 "locationName" string?
                                                 "jsonvalue"    true?})))}
          :opt {"required"   (spec/coll-of string?)
                "payload"    string?
                "deprecated" boolean?})
        (shapes input-shape)))
    `(do
       (defn ~varname                                       ; TODO add deprecated flag
         ~@(when default-arg `[([] (~varname ~default-arg))])
         ([~input]
           (aws/-rest-json-call
             ~(symbol ns "endpoints")
             ~method ~requestUri ~input ~input-spec
             {:headers     ~(into {} (for [[name member] (get-in shapes [input-shape "members"])
                                           :when (= "header" (member "location"))]
                                       [name [(member "locationName") (member "jsonvalue")]]))
              :uri         ~(into {} (for [[name member] (get-in shapes [input-shape "members"])
                                           :when (= "uri" (member "location"))]
                                       [(member "locationName") name]))
              :querystring ~(into {} (for [[name member] (get-in shapes [input-shape "members"])
                                           :when (= "querystring" (member "location"))]
                                       [(member "locationName") name]))
              :payload     ~(get-in shapes [input-shape "payload"])
              :move        ~(into {} (for [[name member] (get-in shapes [input-shape "members"])
                                           :let [locationName (member "locationName")]
                                           :when (when-not (member "location") locationName)]
                                       [locationName name]))}
             ~responseCode ~output-spec ~error-specs)))
       (spec/fdef ~varname
                  :args ~(if input-spec
                           `(~(if default-arg `spec/? `spec/tuple) ~input-spec)
                           `empty?)
                  :ret ~(if output-spec `(spec/and ~output-spec) `true?)))))

(defn gen-api [ns-sym resource-name]
  (let [api (json/parse-stream (-> resource-name io/resource io/reader))]
    (case (get-in api ["metadata" "protocol"])
      "rest-json" (for [[k gen] {"shapes"     (comp #(doto % eval) gen-shape-spec) ; eval to make specs available right away
                                 "operations" (fn [ns [_ op]] (gen-operation ns op (api "shapes")))}
                        desc (api k)]
                    (gen (name ns-sym) desc))
      (do
        (binding [*out* *err*] (prn 'skipping ns-sym 'protocol (get-in api ["metadata" "protocol"])))
        [(list 'comment 'TODO 'support (get-in api ["metadata" "protocol"]))])
      #_"json"
      #_"ec2"
      #_"query"
      #_"rest-xml")))

(defn parse-endpoints! [src]
  (let [endpoints (with-open [r (io/reader src)] (json/parse-stream r))]
    (reduce (fn [m [p v]] (assoc-in m p v)) {}
            (for [{:as   partition
                   :strs [defaults services regions dnsSuffix]} (endpoints "partitions")
                  :let [regions (set (keys regions))]
                  [service {defaults' "defaults" :strs [endpoints partitionEndpoint]}] services
                  :let [defaults (into defaults defaults')]
                  region (into regions (keys endpoints))
                  :let [desc (or (endpoints region) (endpoints partitionEndpoint))]
                  :when desc
                  :let [{:strs [hostname sslCommonName protocols credentialScope signatureVersions]} (into defaults desc)
                        protocol (or (some #{"https"} protocols) (first protocols)) ; prefer https
                        credentialScope (into {"service" service "region" region} credentialScope)
                        sslCommonName (or sslCommonName hostname)
                        env #(case % "{region}" region "{service}" service "{dnsSuffix}" dnsSuffix)
                        hostname (str/replace hostname #"\{(?:region|service|dnsSuffix)}" env)
                        sslCommonName (str/replace sslCommonName #"\{(?:region|service|dnsSuffix)}" env)
                        endpoint (str protocol "://" hostname)
                        signature-version (some->
                                            (or (some (set signatureVersions) ["v4" "s3v4" "v2" "s3"]) ; most recent first
                                                (first signatureVersions))
                                            keyword)]]
              [[service region] {:credential-scope (x/into {} (x/for [[k v] %] [(keyword k) v]) credentialScope)
                                 :ssl-common-name  sslCommonName :endpoint endpoint :signature-version signature-version}]))))

(defn generate-files! []
  (let [endpoints (parse-endpoints! "resources/aws-partitions/partitions.json")]
    (->> (java.io.File. "resources/aws-sdk-core/apis/")
         file-seq
         (into []
               (comp
                 (filter #(= "api-2.json" (.getName ^java.io.File %)))
                 (x/by-key (fn [^java.io.File f] (-> f .getParentFile .getParentFile .getName))
                           (comp (x/for [^java.io.File f %]
                                        [(-> f .getParentFile .getName) (.getPath f)])
                                 (x/into (sorted-map))))
                 (x/for [[api versions] %
                         :let [apifile (str/replace api #"[-.]" "_")
                               apins (str/replace api #"[.]" "-")
                               [latest f] (first (rseq versions))]
                         [version json] (cons [nil f] versions)
                         :let [_ (prn 'generating api (or version 'LATEST))
                               [_ json] (re-matches #"resources/(.*)" json)
                               [file ns]
                               (if version
                                 [(java.io.File. (str "src/portkey/aws/" apifile "/_" version ".clj"))
                                  (symbol (str "portkey.aws." apins ".-" version))]
                                 [(java.io.File. (str "src/portkey/aws/" apifile ".clj"))
                                  (symbol (str "portkey.aws." apins))])]]
                        (with-open [w (io/writer (doto file (-> .getParentFile .mkdirs)))]
                          (binding [*out* w]
                            (prn (list 'ns ns '(:require [portkey.aws])))
                            (newline)
                            (clojure.pprint/pprint (list 'def 'endpoints (list 'quote (get endpoints apins))))
                            (doseq [form (gen-api ns json)]
                              (newline)
                              (if (and (seq? form) (= 'do (first form)))
                                (run! prn (next form))
                                (prn form))))
                          file)))))))




;; SimpleDB operations
(def sdb-operations
  {"ListDomains"           {"name"   "ListDomains",
                            "http"   {"method" "POST", "requestUri" "/"},
                            "input"  {"shape" "ListDomainsRequest"},
                            "output" {"shape" "ListDomainsResult", "resultWrapper" "ListDomainsResult"},
                            "errors" [{"shape"     "InvalidParameterValue",
                                       "error"     {"code" "InvalidParameterValue", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "InvalidNextToken",
                                       "error"     {"code" "InvalidNextToken", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}]},
   "GetAttributes"         {"name"   "GetAttributes",
                            "http"   {"method" "POST", "requestUri" "/"},
                            "input"  {"shape" "GetAttributesRequest"},
                            "output" {"shape" "GetAttributesResult", "resultWrapper" "GetAttributesResult"},
                            "errors" [{"shape"     "InvalidParameterValue",
                                       "error"     {"code" "InvalidParameterValue", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "MissingParameter",
                                       "error"     {"code" "MissingParameter", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "NoSuchDomain",
                                       "error"     {"code" "NoSuchDomain", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}]},
   "BatchDeleteAttributes" {"name"  "BatchDeleteAttributes",
                            "http"  {"method" "POST", "requestUri" "/"},
                            "input" {"shape" "BatchDeleteAttributesRequest"}},
   "DomainMetadata"        {"name"   "DomainMetadata",
                            "http"   {"method" "POST", "requestUri" "/"},
                            "input"  {"shape" "DomainMetadataRequest"},
                            "output" {"shape" "DomainMetadataResult", "resultWrapper" "DomainMetadataResult"},
                            "errors" [{"shape"     "MissingParameter",
                                       "error"     {"code" "MissingParameter", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "NoSuchDomain",
                                       "error"     {"code" "NoSuchDomain", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}]},
   "DeleteAttributes"      {"name"   "DeleteAttributes",
                            "http"   {"method" "POST", "requestUri" "/"},
                            "input"  {"shape" "DeleteAttributesRequest"},
                            "errors" [{"shape"     "InvalidParameterValue",
                                       "error"     {"code" "InvalidParameterValue", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "MissingParameter",
                                       "error"     {"code" "MissingParameter", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "NoSuchDomain",
                                       "error"     {"code" "NoSuchDomain", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "AttributeDoesNotExist",
                                       "error"     {"code" "AttributeDoesNotExist", "httpStatusCode" 404, "senderFault" true},
                                       "exception" true}]},
   "PutAttributes"         {"name"   "PutAttributes",
                            "http"   {"method" "POST", "requestUri" "/"},
                            "input"  {"shape" "PutAttributesRequest"},
                            "errors" [{"shape"     "InvalidParameterValue",
                                       "error"     {"code" "InvalidParameterValue", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "MissingParameter",
                                       "error"     {"code" "MissingParameter", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "NoSuchDomain",
                                       "error"     {"code" "NoSuchDomain", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "NumberDomainAttributesExceeded",
                                       "error"     {"code" "NumberDomainAttributesExceeded", "httpStatusCode" 409, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "NumberDomainBytesExceeded",
                                       "error"     {"code" "NumberDomainBytesExceeded", "httpStatusCode" 409, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "NumberItemAttributesExceeded",
                                       "error"     {"code" "NumberItemAttributesExceeded", "httpStatusCode" 409, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "AttributeDoesNotExist",
                                       "error"     {"code" "AttributeDoesNotExist", "httpStatusCode" 404, "senderFault" true},
                                       "exception" true}]},
   "CreateDomain"          {"name"   "CreateDomain",
                            "http"   {"method" "POST", "requestUri" "/"},
                            "input"  {"shape" "CreateDomainRequest"},
                            "errors" [{"shape"     "InvalidParameterValue",
                                       "error"     {"code" "InvalidParameterValue", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "MissingParameter",
                                       "error"     {"code" "MissingParameter", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "NumberDomainsExceeded",
                                       "error"     {"code" "NumberDomainsExceeded", "httpStatusCode" 409, "senderFault" true},
                                       "exception" true}]},
   "BatchPutAttributes"    {"name"   "BatchPutAttributes",
                            "http"   {"method" "POST", "requestUri" "/"},
                            "input"  {"shape" "BatchPutAttributesRequest"},
                            "errors" [{"shape"     "DuplicateItemName",
                                       "error"     {"code" "DuplicateItemName", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "InvalidParameterValue",
                                       "error"     {"code" "InvalidParameterValue", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "MissingParameter",
                                       "error"     {"code" "MissingParameter", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "NoSuchDomain",
                                       "error"     {"code" "NoSuchDomain", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "NumberItemAttributesExceeded",
                                       "error"     {"code"           "NumberItemAttributesExceeded",
                                                    "httpStatusCode" 409,
                                                    "senderFault"    true},
                                       "exception" true}
                                      {"shape"     "NumberDomainAttributesExceeded",
                                       "error"     {"code"           "NumberDomainAttributesExceeded",
                                                    "httpStatusCode" 409,
                                                    "senderFault"    true},
                                       "exception" true}
                                      {"shape"     "NumberDomainBytesExceeded",
                                       "error"     {"code" "NumberDomainBytesExceeded", "httpStatusCode" 409, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "NumberSubmittedItemsExceeded",
                                       "error"     {"code"           "NumberSubmittedItemsExceeded",
                                                    "httpStatusCode" 409,
                                                    "senderFault"    true},
                                       "exception" true}
                                      {"shape"     "NumberSubmittedAttributesExceeded",
                                       "error"     {"code"           "NumberSubmittedAttributesExceeded",
                                                    "httpStatusCode" 409,
                                                    "senderFault"    true},
                                       "exception" true}]},
   "DeleteDomain"          {"name"   "DeleteDomain",
                            "http"   {"method" "POST", "requestUri" "/"},
                            "input"  {"shape" "DeleteDomainRequest"},
                            "errors" [{"shape"     "MissingParameter",
                                       "error"     {"code" "MissingParameter", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}]},
   "Select"                {"name"   "Select",
                            "http"   {"method" "POST", "requestUri" "/"},
                            "input"  {"shape" "SelectRequest"},
                            "output" {"shape" "SelectResult", "resultWrapper" "SelectResult"},
                            "errors" [{"shape"     "InvalidParameterValue",
                                       "error"     {"code" "InvalidParameterValue", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "InvalidNextToken",
                                       "error"     {"code" "InvalidNextToken", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "InvalidNumberPredicates",
                                       "error"     {"code" "InvalidNumberPredicates", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "InvalidNumberValueTests",
                                       "error"     {"code" "InvalidNumberValueTests", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "InvalidQueryExpression",
                                       "error"     {"code" "InvalidQueryExpression", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "MissingParameter",
                                       "error"     {"code" "MissingParameter", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "NoSuchDomain",
                                       "error"     {"code" "NoSuchDomain", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "RequestTimeout",
                                       "error"     {"code" "RequestTimeout", "httpStatusCode" 408, "senderFault" true},
                                       "exception" true}
                                      {"shape"     "TooManyRequestedAttributes",
                                       "error"     {"code" "TooManyRequestedAttributes", "httpStatusCode" 400, "senderFault" true},
                                       "exception" true}]}})

;; SimpleDB Shapes
(def sdb-shapes
  {"InvalidQueryExpression"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "InvalidQueryExpression",
                           "httpStatusCode" 400,
                           "senderFault"    true},
              "exception" true},
   "DeleteAttributesRequest"
             {"type"     "structure",
              "required" ["DomainName" "ItemName"],
              "members"
                         {"DomainName" {"shape" "String"},
                          "ItemName"   {"shape" "String"},
                          "Attributes" {"shape" "AttributeList"},
                          "Expected"   {"shape" "UpdateCondition"}}},
   "AttributeList"
             {"type"      "list",
              "member"    {"shape" "Attribute", "locationName" "Attribute"},
              "flattened" true},
   "DuplicateItemName"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "DuplicateItemName",
                           "httpStatusCode" 400,
                           "senderFault"    true},
              "exception" true},
   "InvalidNumberPredicates"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "InvalidNumberPredicates",
                           "httpStatusCode" 400,
                           "senderFault"    true},
              "exception" true},
   "DeletableItemList"
             {"type"      "list",
              "member"    {"shape" "DeletableItem", "locationName" "Item"},
              "flattened" true},
   "DeletableItem"
             {"type"     "structure",
              "required" ["Name"],
              "members"
                         {"Name"       {"shape" "String", "locationName" "ItemName"},
                          "Attributes" {"shape" "AttributeList"}}},
   "NumberSubmittedAttributesExceeded"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "NumberSubmittedAttributesExceeded",
                           "httpStatusCode" 409,
                           "senderFault"    true},
              "exception" true},
   "SelectResult"
             {"type" "structure",
              "members"
                     {"Items" {"shape" "ItemList"}, "NextToken" {"shape" "String"}}},
   "ReplaceableItem"
             {"type"     "structure",
              "required" ["Name" "Attributes"],
              "members"
                         {"Name"       {"shape" "String", "locationName" "ItemName"},
                          "Attributes" {"shape" "ReplaceableAttributeList"}}},
   "DomainMetadataRequest"
             {"type"     "structure",
              "required" ["DomainName"],
              "members"  {"DomainName" {"shape" "String"}}},
   "InvalidParameterValue"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "InvalidParameterValue",
                           "httpStatusCode" 400,
                           "senderFault"    true},
              "exception" true},
   "BatchPutAttributesRequest"
             {"type"     "structure",
              "required" ["DomainName" "Items"],
              "members"
                         {"DomainName" {"shape" "String"},
                          "Items"      {"shape" "ReplaceableItemList"}}},
   "PutAttributesRequest"
             {"type"     "structure",
              "required" ["DomainName" "ItemName" "Attributes"],
              "members"
                         {"DomainName" {"shape" "String"},
                          "ItemName"   {"shape" "String"},
                          "Attributes" {"shape" "ReplaceableAttributeList"},
                          "Expected"   {"shape" "UpdateCondition"}}},
   "CreateDomainRequest"
             {"type"     "structure",
              "required" ["DomainName"],
              "members"  {"DomainName" {"shape" "String"}}},
   "NumberDomainBytesExceeded"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "NumberDomainBytesExceeded",
                           "httpStatusCode" 409,
                           "senderFault"    true},
              "exception" true},
   "NumberDomainAttributesExceeded"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "NumberDomainAttributesExceeded",
                           "httpStatusCode" 409,
                           "senderFault"    true},
              "exception" true},
   "ItemList"
             {"type"      "list",
              "member"    {"shape" "Item", "locationName" "Item"},
              "flattened" true},
   "BatchDeleteAttributesRequest"
             {"type"     "structure",
              "required" ["DomainName" "Items"],
              "members"
                         {"DomainName" {"shape" "String"},
                          "Items"      {"shape" "DeletableItemList"}}},
   "Attribute"
             {"type"     "structure",
              "required" ["Name" "Value"],
              "members"
                         {"Name"                   {"shape" "String"},
                          "AlternateNameEncoding"  {"shape" "String"},
                          "Value"                  {"shape" "String"},
                          "AlternateValueEncoding" {"shape" "String"}}},
   "ReplaceableAttributeList"
             {"type"      "list",
              "member"
                          {"shape" "ReplaceableAttribute", "locationName" "Attribute"},
              "flattened" true},
   "DomainNameList"
             {"type"      "list",
              "member"    {"shape" "String", "locationName" "DomainName"},
              "flattened" true},
   "UpdateCondition"
             {"type" "structure",
              "members"
                     {"Name"   {"shape" "String"},
                      "Value"  {"shape" "String"},
                      "Exists" {"shape" "Boolean"}}},
   "DeleteDomainRequest"
             {"type"     "structure",
              "required" ["DomainName"],
              "members"  {"DomainName" {"shape" "String"}}},
   "NoSuchDomain"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code" "NoSuchDomain", "httpStatusCode" 400, "senderFault" true},
              "exception" true},
   "ListDomainsRequest"
             {"type" "structure",
              "members"
                     {"MaxNumberOfDomains" {"shape" "Integer"},
                      "NextToken"          {"shape" "String"}}},
   "NumberDomainsExceeded"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "NumberDomainsExceeded",
                           "httpStatusCode" 409,
                           "senderFault"    true},
              "exception" true},
   "ListDomainsResult"
             {"type" "structure",
              "members"
                     {"DomainNames" {"shape" "DomainNameList"},
                      "NextToken"   {"shape" "String"}}},
   "AttributeNameList"
             {"type"      "list",
              "member"    {"shape" "String", "locationName" "AttributeName"},
              "flattened" true},
   "Integer" {"type" "integer"},
   "String"  {"type" "string"},
   "ReplaceableAttribute"
             {"type"     "structure",
              "required" ["Name" "Value"],
              "members"
                         {"Name"    {"shape" "String"},
                          "Value"   {"shape" "String"},
                          "Replace" {"shape" "Boolean"}}},
   "GetAttributesRequest"
             {"type"     "structure",
              "required" ["DomainName" "ItemName"],
              "members"
                         {"DomainName"     {"shape" "String"},
                          "ItemName"       {"shape" "String"},
                          "AttributeNames" {"shape" "AttributeNameList"},
                          "ConsistentRead" {"shape" "Boolean"}}},
   "Long"    {"type" "long"},
   "ReplaceableItemList"
             {"type"      "list",
              "member"    {"shape" "ReplaceableItem", "locationName" "Item"},
              "flattened" true},
   "SelectRequest"
             {"type"     "structure",
              "required" ["SelectExpression"],
              "members"
                         {"SelectExpression" {"shape" "String"},
                          "NextToken"        {"shape" "String"},
                          "ConsistentRead"   {"shape" "Boolean"}}},
   "AttributeDoesNotExist"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "AttributeDoesNotExist",
                           "httpStatusCode" 404,
                           "senderFault"    true},
              "exception" true},
   "NumberSubmittedItemsExceeded"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "NumberSubmittedItemsExceeded",
                           "httpStatusCode" 409,
                           "senderFault"    true},
              "exception" true},
   "InvalidNumberValueTests"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "InvalidNumberValueTests",
                           "httpStatusCode" 400,
                           "senderFault"    true},
              "exception" true},
   "NumberItemAttributesExceeded"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "NumberItemAttributesExceeded",
                           "httpStatusCode" 409,
                           "senderFault"    true},
              "exception" true},
   "DomainMetadataResult"
             {"type" "structure",
              "members"
                     {"ItemCount"                {"shape" "Integer"},
                      "ItemNamesSizeBytes"       {"shape" "Long"},
                      "AttributeNameCount"       {"shape" "Integer"},
                      "AttributeNamesSizeBytes"  {"shape" "Long"},
                      "AttributeValueCount"      {"shape" "Integer"},
                      "AttributeValuesSizeBytes" {"shape" "Long"},
                      "Timestamp"                {"shape" "Integer"}}},
   "GetAttributesResult"
             {"type"    "structure",
              "members" {"Attributes" {"shape" "AttributeList"}}},
   "TooManyRequestedAttributes"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "TooManyRequestedAttributes",
                           "httpStatusCode" 400,
                           "senderFault"    true},
              "exception" true},
   "RequestTimeout"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code" "RequestTimeout", "httpStatusCode" 408, "senderFault" true},
              "exception" true},
   "Item"
             {"type"     "structure",
              "required" ["Name" "Attributes"],
              "members"
                         {"Name"                  {"shape" "String"},
                          "AlternateNameEncoding" {"shape" "String"},
                          "Attributes"            {"shape" "AttributeList"}}},
   "Float"   {"type" "float"},
   "MissingParameter"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "MissingParameter",
                           "httpStatusCode" 400,
                           "senderFault"    true},
              "exception" true},
   "InvalidNextToken"
             {"type"      "structure",
              "members"   {"BoxUsage" {"shape" "Float"}},
              "error"
                          {"code"           "InvalidNextToken",
                           "httpStatusCode" 400,
                           "senderFault"    true},
              "exception" true},
   "Boolean" {"type" "boolean"}})


(defn caca [ns {:as                                      operation
                         :strs                                    [name errors]
                         {input-shape "shape"}                    "input"
                         {output-shape "shape"}                   "output"
                         {:strs [method requestUri responseCode]} "http"}
                     shapes]
  (let [error-specs (into {}
                          (map (fn [{:strs [shape] {:strs [httpStatusCode]} "error"}]
                                 [shape (keyword ns (aws/dashed shape))]))
                          errors)
        varname (symbol (aws/dashed name))
        input-spec (some->> input-shape aws/dashed (keyword ns))
        output-spec (some->> output-shape aws/dashed (keyword ns))
        input (or (some-> input-shape aws/dashed symbol) '_)
        default-arg (if input-spec (some #(when (spec/valid? input-spec %) %) [[] {}]) {})]
    (when input-shape
      (aws/conform-or-throw
        (strict-strs                                        ; validate only what we knows how to map
          :req {"type"    #{"structure"}
                "members" (spec/map-of string?
                                       (spec/or
                                         :atomic
                                         (spec/and
                                           (strict-strs
                                             :req {"shape" string?}
                                             :opt {"location"     #{"uri" "querystring" "header" #_#_"headers" "statusCode"}
                                                   "locationName" string?
                                                   "deprecated"   boolean?})
                                           #(= (contains? % "location") (contains? % "locationName")))
                                         :querystringmap
                                         (strict-strs
                                           :req {"shape" string?}
                                           :opt {"location" #{"querystring"}})
                                         :move
                                         (strict-strs
                                           :req {"shape" string?}
                                           :opt {"locationName" string?})
                                         :json-value
                                         (strict-strs
                                           :req {"shape"        string?
                                                 "location"     #{"header"}
                                                 "locationName" string?
                                                 "jsonvalue"    true?})))}
          :opt {"required"   (spec/coll-of string?)
                "payload"    string?
                "deprecated" boolean?})
        (shapes input-shape)))
    `(do
       (defn ~varname                                       ; TODO add deprecated flag
         ~@(when default-arg `[([] (~varname ~default-arg))])
         ([~input]
           (aws/-query-call
             ~(symbol ns "endpoints")
             ~method ~requestUri ~input ~input-spec
             {:headers     ~(into {} (for [[name member] (get-in shapes [input-shape "members"])
                                           :when (= "header" (member "location"))]
                                       [name [(member "locationName") (member "jsonvalue")]]))
              :uri         ~(into {} (for [[name member] (get-in shapes [input-shape "members"])
                                           :when (= "uri" (member "location"))]
                                       [(member "locationName") name]))
              :querystring ~(into {} (for [[name member] (get-in shapes [input-shape "members"])
                                           :when (= "querystring" (member "location"))]
                                       [(member "locationName") name]))
              :payload     ~(get-in shapes [input-shape "payload"])
              :move        ~(into {} (for [[name member] (get-in shapes [input-shape "members"])
                                           :let [locationName (member "locationName")]
                                           :when (when-not (member "location") locationName)]
                                       [locationName name]))}
             ~responseCode ~output-spec ~error-specs)))
       (spec/fdef ~varname
                  :args ~(if input-spec
                           `(~(if default-arg `spec/? `spec/tuple) ~input-spec)
                           `empty?)
                  :ret ~(if output-spec `(spec/and ~output-spec) `true?)))))

(comment
  (let [api (json/parse-stream (-> "aws-sdk-core/apis/sdb/2009-04-15/api-2.json" io/resource io/reader))]
    (case (get-in api ["metadata" "protocol"])
      "rest-json" (for [[k gen] {"shapes"     (comp #(doto % eval) gen-shape-spec) ; eval to make specs available right away
                                 "operations" (fn [ns [_ op]] (gen-operation ns op (api "shapes")))}
                        desc (api k)]
                    (println "caca")
                    #_(gen (name ns-sym) desc))
      "query" (for [[k gen] {"shapes"     (comp #(doto % eval) gen-shape-spec)
                             "operations" (fn [ns [_ op]] (caca ns op (api "shapes")))}
                    desc (api k)]
                (gen (name (ns-name *ns*)) desc))
      #_"json"
      #_"ec2"
      #_"query"
      #_"rest-xml")))





(def
  endpoints
  '{"ap-northeast-1"
    {:credential-scope {:service "sdb", :region "ap-northeast-1"},
     :ssl-common-name "sdb.ap-northeast-1.amazonaws.com",
     :endpoint "https://sdb.ap-northeast-1.amazonaws.com",
     :signature-version :v2},
    "eu-west-1"
    {:credential-scope {:service "sdb", :region "eu-west-1"},
     :ssl-common-name "sdb.eu-west-1.amazonaws.com",
     :endpoint "https://sdb.eu-west-1.amazonaws.com",
     :signature-version :v2},
    "ap-southeast-2"
    {:credential-scope {:service "sdb", :region "ap-southeast-2"},
     :ssl-common-name "sdb.ap-southeast-2.amazonaws.com",
     :endpoint "https://sdb.ap-southeast-2.amazonaws.com",
     :signature-version :v2},
    "sa-east-1"
    {:credential-scope {:service "sdb", :region "sa-east-1"},
     :ssl-common-name "sdb.sa-east-1.amazonaws.com",
     :endpoint "https://sdb.sa-east-1.amazonaws.com",
     :signature-version :v2},
    "ap-southeast-1"
    {:credential-scope {:service "sdb", :region "ap-southeast-1"},
     :ssl-common-name "sdb.ap-southeast-1.amazonaws.com",
     :endpoint "https://sdb.ap-southeast-1.amazonaws.com",
     :signature-version :v2},
    "us-west-2"
    {:credential-scope {:service "sdb", :region "us-west-2"},
     :ssl-common-name "sdb.us-west-2.amazonaws.com",
     :endpoint "https://sdb.us-west-2.amazonaws.com",
     :signature-version :v2},
    "us-east-1"
    {:credential-scope {:service "sdb", :region "us-east-1"},
     :ssl-common-name "sdb.amazonaws.com",
     :endpoint "https://sdb.amazonaws.com",
     :signature-version :v2},
    "us-west-1"
    {:credential-scope {:service "sdb", :region "us-west-1"},
     :ssl-common-name "sdb.us-west-1.amazonaws.com",
     :endpoint "https://sdb.us-west-1.amazonaws.com",
     :signature-version :v2}})

(do
  (clojure.spec.alpha/def
    :portkey.awsgen.invalid-query-expression/box-usage
    (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/invalid-query-expression
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.invalid-query-expression/BoxUsage] :locations {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.delete-attributes-request/domain-name
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen.delete-attributes-request/item-name
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen.delete-attributes-request/attributes
    (clojure.spec.alpha/and :portkey.awsgen/attribute-list))
  (clojure.spec.alpha/def
    :portkey.awsgen.delete-attributes-request/expected
    (clojure.spec.alpha/and :portkey.awsgen/update-condition))
  (clojure.spec.alpha/def
    :portkey.awsgen/delete-attributes-request
    (portkey.aws/json-keys
      :req-un
      [:portkey.awsgen.delete-attributes-request/DomainName :portkey.awsgen.delete-attributes-request/ItemName]
      :opt-un
      [:portkey.awsgen.delete-attributes-request/Attributes :portkey.awsgen.delete-attributes-request/Expected]
      :locations
      {})))
(clojure.spec.alpha/def
  :portkey.awsgen/attribute-list
  (clojure.spec.alpha/and
    (clojure.spec.alpha/coll-of :portkey.awsgen/attribute)
    (clojure.spec.alpha/conformer
      clojure.core/identity
      (fn*
        [p1__15046__15047__auto__]
        (if (clojure.core/sequential? p1__15046__15047__auto__) p1__15046__15047__auto__ [p1__15046__15047__auto__])))))
(do
  (clojure.spec.alpha/def :portkey.awsgen.duplicate-item-name/box-usage (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/duplicate-item-name
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.duplicate-item-name/BoxUsage] :locations {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.invalid-number-predicates/box-usage
    (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/invalid-number-predicates
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.invalid-number-predicates/BoxUsage] :locations {})))
(clojure.spec.alpha/def
  :portkey.awsgen/deletable-item-list
  (clojure.spec.alpha/and
    (clojure.spec.alpha/coll-of :portkey.awsgen/deletable-item)
    (clojure.spec.alpha/conformer
      clojure.core/identity
      (fn*
        [p1__15046__15047__auto__]
        (if (clojure.core/sequential? p1__15046__15047__auto__) p1__15046__15047__auto__ [p1__15046__15047__auto__])))))
(do
  (clojure.spec.alpha/def :portkey.awsgen.deletable-item/name (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen.deletable-item/attributes
    (clojure.spec.alpha/and :portkey.awsgen/attribute-list))
  (clojure.spec.alpha/def
    :portkey.awsgen/deletable-item
    (portkey.aws/json-keys
      :req-un
      [:portkey.awsgen.deletable-item/Name]
      :opt-un
      [:portkey.awsgen.deletable-item/Attributes]
      :locations
      {"ItemName" "Name"})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.number-submitted-attributes-exceeded/box-usage
    (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/number-submitted-attributes-exceeded
    (portkey.aws/json-keys
      :req-un
      []
      :opt-un
      [:portkey.awsgen.number-submitted-attributes-exceeded/BoxUsage]
      :locations
      {})))
(do
  (clojure.spec.alpha/def :portkey.awsgen.select-result/items (clojure.spec.alpha/and :portkey.awsgen/item-list))
  (clojure.spec.alpha/def :portkey.awsgen.select-result/next-token (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen/select-result
    (portkey.aws/json-keys
      :req-un
      []
      :opt-un
      [:portkey.awsgen.select-result/Items :portkey.awsgen.select-result/NextToken]
      :locations
      {})))
(do
  (clojure.spec.alpha/def :portkey.awsgen.replaceable-item/name (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen.replaceable-item/attributes
    (clojure.spec.alpha/and :portkey.awsgen/replaceable-attribute-list))
  (clojure.spec.alpha/def
    :portkey.awsgen/replaceable-item
    (portkey.aws/json-keys
      :req-un
      [:portkey.awsgen.replaceable-item/Name :portkey.awsgen.replaceable-item/Attributes]
      :opt-un
      []
      :locations
      {"ItemName" "Name"})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.domain-metadata-request/domain-name
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen/domain-metadata-request
    (portkey.aws/json-keys :req-un [:portkey.awsgen.domain-metadata-request/DomainName] :opt-un [] :locations {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.invalid-parameter-value/box-usage
    (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/invalid-parameter-value
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.invalid-parameter-value/BoxUsage] :locations {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.batch-put-attributes-request/domain-name
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen.batch-put-attributes-request/items
    (clojure.spec.alpha/and :portkey.awsgen/replaceable-item-list))
  (clojure.spec.alpha/def
    :portkey.awsgen/batch-put-attributes-request
    (portkey.aws/json-keys
      :req-un
      [:portkey.awsgen.batch-put-attributes-request/DomainName :portkey.awsgen.batch-put-attributes-request/Items]
      :opt-un
      []
      :locations
      {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.put-attributes-request/domain-name
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen.put-attributes-request/item-name
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen.put-attributes-request/attributes
    (clojure.spec.alpha/and :portkey.awsgen/replaceable-attribute-list))
  (clojure.spec.alpha/def
    :portkey.awsgen.put-attributes-request/expected
    (clojure.spec.alpha/and :portkey.awsgen/update-condition))
  (clojure.spec.alpha/def
    :portkey.awsgen/put-attributes-request
    (portkey.aws/json-keys
      :req-un
      [:portkey.awsgen.put-attributes-request/DomainName
       :portkey.awsgen.put-attributes-request/ItemName
       :portkey.awsgen.put-attributes-request/Attributes]
      :opt-un
      [:portkey.awsgen.put-attributes-request/Expected]
      :locations
      {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.create-domain-request/domain-name
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen/create-domain-request
    (portkey.aws/json-keys :req-un [:portkey.awsgen.create-domain-request/DomainName] :opt-un [] :locations {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.number-domain-bytes-exceeded/box-usage
    (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/number-domain-bytes-exceeded
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.number-domain-bytes-exceeded/BoxUsage] :locations {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.number-domain-attributes-exceeded/box-usage
    (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/number-domain-attributes-exceeded
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.number-domain-attributes-exceeded/BoxUsage] :locations {})))
(clojure.spec.alpha/def
  :portkey.awsgen/item-list
  (clojure.spec.alpha/and
    (clojure.spec.alpha/coll-of :portkey.awsgen/item)
    (clojure.spec.alpha/conformer
      clojure.core/identity
      (fn*
        [p1__15046__15047__auto__]
        (if (clojure.core/sequential? p1__15046__15047__auto__) p1__15046__15047__auto__ [p1__15046__15047__auto__])))))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.batch-delete-attributes-request/domain-name
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen.batch-delete-attributes-request/items
    (clojure.spec.alpha/and :portkey.awsgen/deletable-item-list))
  (clojure.spec.alpha/def
    :portkey.awsgen/batch-delete-attributes-request
    (portkey.aws/json-keys
      :req-un
      [:portkey.awsgen.batch-delete-attributes-request/DomainName :portkey.awsgen.batch-delete-attributes-request/Items]
      :opt-un
      []
      :locations
      {})))
(do
  (clojure.spec.alpha/def :portkey.awsgen.attribute/name (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen.attribute/alternate-name-encoding
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def :portkey.awsgen.attribute/value (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen.attribute/alternate-value-encoding
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen/attribute
    (portkey.aws/json-keys
      :req-un
      [:portkey.awsgen.attribute/Name :portkey.awsgen.attribute/Value]
      :opt-un
      [:portkey.awsgen.attribute/AlternateNameEncoding :portkey.awsgen.attribute/AlternateValueEncoding]
      :locations
      {})))
(clojure.spec.alpha/def
  :portkey.awsgen/replaceable-attribute-list
  (clojure.spec.alpha/and
    (clojure.spec.alpha/coll-of :portkey.awsgen/replaceable-attribute)
    (clojure.spec.alpha/conformer
      clojure.core/identity
      (fn*
        [p1__15046__15047__auto__]
        (if (clojure.core/sequential? p1__15046__15047__auto__) p1__15046__15047__auto__ [p1__15046__15047__auto__])))))
(clojure.spec.alpha/def
  :portkey.awsgen/domain-name-list
  (clojure.spec.alpha/and
    (clojure.spec.alpha/coll-of :portkey.awsgen/string)
    (clojure.spec.alpha/conformer
      clojure.core/identity
      (fn*
        [p1__15046__15047__auto__]
        (if (clojure.core/sequential? p1__15046__15047__auto__) p1__15046__15047__auto__ [p1__15046__15047__auto__])))))
(do
  (clojure.spec.alpha/def :portkey.awsgen.update-condition/name (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def :portkey.awsgen.update-condition/value (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def :portkey.awsgen.update-condition/exists (clojure.spec.alpha/and :portkey.awsgen/boolean))
  (clojure.spec.alpha/def
    :portkey.awsgen/update-condition
    (portkey.aws/json-keys
      :req-un
      []
      :opt-un
      [:portkey.awsgen.update-condition/Name
       :portkey.awsgen.update-condition/Value
       :portkey.awsgen.update-condition/Exists]
      :locations
      {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.delete-domain-request/domain-name
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen/delete-domain-request
    (portkey.aws/json-keys :req-un [:portkey.awsgen.delete-domain-request/DomainName] :opt-un [] :locations {})))
(do
  (clojure.spec.alpha/def :portkey.awsgen.no-such-domain/box-usage (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/no-such-domain
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.no-such-domain/BoxUsage] :locations {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.list-domains-request/max-number-of-domains
    (clojure.spec.alpha/and :portkey.awsgen/integer))
  (clojure.spec.alpha/def
    :portkey.awsgen.list-domains-request/next-token
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen/list-domains-request
    (portkey.aws/json-keys
      :req-un
      []
      :opt-un
      [:portkey.awsgen.list-domains-request/MaxNumberOfDomains :portkey.awsgen.list-domains-request/NextToken]
      :locations
      {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.number-domains-exceeded/box-usage
    (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/number-domains-exceeded
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.number-domains-exceeded/BoxUsage] :locations {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.list-domains-result/domain-names
    (clojure.spec.alpha/and :portkey.awsgen/domain-name-list))
  (clojure.spec.alpha/def
    :portkey.awsgen.list-domains-result/next-token
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen/list-domains-result
    (portkey.aws/json-keys
      :req-un
      []
      :opt-un
      [:portkey.awsgen.list-domains-result/DomainNames :portkey.awsgen.list-domains-result/NextToken]
      :locations
      {})))
(clojure.spec.alpha/def
  :portkey.awsgen/attribute-name-list
  (clojure.spec.alpha/and
    (clojure.spec.alpha/coll-of :portkey.awsgen/string)
    (clojure.spec.alpha/conformer
      clojure.core/identity
      (fn*
        [p1__15046__15047__auto__]
        (if (clojure.core/sequential? p1__15046__15047__auto__) p1__15046__15047__auto__ [p1__15046__15047__auto__])))))
(clojure.spec.alpha/def :portkey.awsgen/integer (clojure.spec.alpha/and clojure.core/int?))
(clojure.spec.alpha/def :portkey.awsgen/string (clojure.spec.alpha/and clojure.core/string?))
(do
  (clojure.spec.alpha/def :portkey.awsgen.replaceable-attribute/name (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def :portkey.awsgen.replaceable-attribute/value (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen.replaceable-attribute/replace
    (clojure.spec.alpha/and :portkey.awsgen/boolean))
  (clojure.spec.alpha/def
    :portkey.awsgen/replaceable-attribute
    (portkey.aws/json-keys
      :req-un
      [:portkey.awsgen.replaceable-attribute/Name :portkey.awsgen.replaceable-attribute/Value]
      :opt-un
      [:portkey.awsgen.replaceable-attribute/Replace]
      :locations
      {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.get-attributes-request/domain-name
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen.get-attributes-request/item-name
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen.get-attributes-request/attribute-names
    (clojure.spec.alpha/and :portkey.awsgen/attribute-name-list))
  (clojure.spec.alpha/def
    :portkey.awsgen.get-attributes-request/consistent-read
    (clojure.spec.alpha/and :portkey.awsgen/boolean))
  (clojure.spec.alpha/def
    :portkey.awsgen/get-attributes-request
    (portkey.aws/json-keys
      :req-un
      [:portkey.awsgen.get-attributes-request/DomainName :portkey.awsgen.get-attributes-request/ItemName]
      :opt-un
      [:portkey.awsgen.get-attributes-request/AttributeNames :portkey.awsgen.get-attributes-request/ConsistentRead]
      :locations
      {})))
(clojure.spec.alpha/def :portkey.awsgen/long clojure.core/int?)
(clojure.spec.alpha/def
  :portkey.awsgen/replaceable-item-list
  (clojure.spec.alpha/and
    (clojure.spec.alpha/coll-of :portkey.awsgen/replaceable-item)
    (clojure.spec.alpha/conformer
      clojure.core/identity
      (fn*
        [p1__15046__15047__auto__]
        (if (clojure.core/sequential? p1__15046__15047__auto__) p1__15046__15047__auto__ [p1__15046__15047__auto__])))))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.select-request/select-expression
    (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def :portkey.awsgen.select-request/next-token (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def
    :portkey.awsgen.select-request/consistent-read
    (clojure.spec.alpha/and :portkey.awsgen/boolean))
  (clojure.spec.alpha/def
    :portkey.awsgen/select-request
    (portkey.aws/json-keys
      :req-un
      [:portkey.awsgen.select-request/SelectExpression]
      :opt-un
      [:portkey.awsgen.select-request/NextToken :portkey.awsgen.select-request/ConsistentRead]
      :locations
      {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.attribute-does-not-exist/box-usage
    (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/attribute-does-not-exist
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.attribute-does-not-exist/BoxUsage] :locations {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.number-submitted-items-exceeded/box-usage
    (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/number-submitted-items-exceeded
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.number-submitted-items-exceeded/BoxUsage] :locations {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.invalid-number-value-tests/box-usage
    (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/invalid-number-value-tests
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.invalid-number-value-tests/BoxUsage] :locations {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.number-item-attributes-exceeded/box-usage
    (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/number-item-attributes-exceeded
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.number-item-attributes-exceeded/BoxUsage] :locations {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.domain-metadata-result/item-count
    (clojure.spec.alpha/and :portkey.awsgen/integer))
  (clojure.spec.alpha/def
    :portkey.awsgen.domain-metadata-result/item-names-size-bytes
    (clojure.spec.alpha/and :portkey.awsgen/long))
  (clojure.spec.alpha/def
    :portkey.awsgen.domain-metadata-result/attribute-name-count
    (clojure.spec.alpha/and :portkey.awsgen/integer))
  (clojure.spec.alpha/def
    :portkey.awsgen.domain-metadata-result/attribute-names-size-bytes
    (clojure.spec.alpha/and :portkey.awsgen/long))
  (clojure.spec.alpha/def
    :portkey.awsgen.domain-metadata-result/attribute-value-count
    (clojure.spec.alpha/and :portkey.awsgen/integer))
  (clojure.spec.alpha/def
    :portkey.awsgen.domain-metadata-result/attribute-values-size-bytes
    (clojure.spec.alpha/and :portkey.awsgen/long))
  (clojure.spec.alpha/def
    :portkey.awsgen.domain-metadata-result/timestamp
    (clojure.spec.alpha/and :portkey.awsgen/integer))
  (clojure.spec.alpha/def
    :portkey.awsgen/domain-metadata-result
    (portkey.aws/json-keys
      :req-un
      []
      :opt-un
      [:portkey.awsgen.domain-metadata-result/ItemCount
       :portkey.awsgen.domain-metadata-result/ItemNamesSizeBytes
       :portkey.awsgen.domain-metadata-result/AttributeNameCount
       :portkey.awsgen.domain-metadata-result/AttributeNamesSizeBytes
       :portkey.awsgen.domain-metadata-result/AttributeValueCount
       :portkey.awsgen.domain-metadata-result/AttributeValuesSizeBytes
       :portkey.awsgen.domain-metadata-result/Timestamp]
      :locations
      {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.get-attributes-result/attributes
    (clojure.spec.alpha/and :portkey.awsgen/attribute-list))
  (clojure.spec.alpha/def
    :portkey.awsgen/get-attributes-result
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.get-attributes-result/Attributes] :locations {})))
(do
  (clojure.spec.alpha/def
    :portkey.awsgen.too-many-requested-attributes/box-usage
    (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/too-many-requested-attributes
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.too-many-requested-attributes/BoxUsage] :locations {})))
(do
  (clojure.spec.alpha/def :portkey.awsgen.request-timeout/box-usage (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/request-timeout
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.request-timeout/BoxUsage] :locations {})))
(do
  (clojure.spec.alpha/def :portkey.awsgen.item/name (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def :portkey.awsgen.item/alternate-name-encoding (clojure.spec.alpha/and :portkey.awsgen/string))
  (clojure.spec.alpha/def :portkey.awsgen.item/attributes (clojure.spec.alpha/and :portkey.awsgen/attribute-list))
  (clojure.spec.alpha/def
    :portkey.awsgen/item
    (portkey.aws/json-keys
      :req-un
      [:portkey.awsgen.item/Name :portkey.awsgen.item/Attributes]
      :opt-un
      [:portkey.awsgen.item/AlternateNameEncoding]
      :locations
      {})))
(clojure.spec.alpha/def :portkey.awsgen/float clojure.core/double?)
(do
  (clojure.spec.alpha/def :portkey.awsgen.missing-parameter/box-usage (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/missing-parameter
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.missing-parameter/BoxUsage] :locations {})))
(do
  (clojure.spec.alpha/def :portkey.awsgen.invalid-next-token/box-usage (clojure.spec.alpha/and :portkey.awsgen/float))
  (clojure.spec.alpha/def
    :portkey.awsgen/invalid-next-token
    (portkey.aws/json-keys :req-un [] :opt-un [:portkey.awsgen.invalid-next-token/BoxUsage] :locations {})))
(clojure.spec.alpha/def :portkey.awsgen/boolean clojure.core/boolean?)



(clojure.spec.alpha/fdef
  create-domain
  :args
  (clojure.spec.alpha/tuple :portkey.awsgen/create-domain-request)
  :ret
  clojure.core/true?)

(clojure.core/defn
  create-domain
  ([create-domain-request]
   (portkey.aws/-query-call
     portkey.awsgen/endpoints
     "POST"
     "/"
     create-domain-request
     :portkey.awsgen/create-domain-request
     {:payload nil, :move {}, :headers {}, :uri {}, :querystring {}}
     nil
     nil
     {"InvalidParameterValue" :portkey.awsgen/invalid-parameter-value,
      "MissingParameter" :portkey.awsgen/missing-parameter,
      "NumberDomainsExceeded" :portkey.awsgen/number-domains-exceeded})))









