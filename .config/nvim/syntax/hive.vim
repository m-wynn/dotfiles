sy case ignore

syn keyword sqlKeyword action add add2 all allocate and ansi_nulls ansi_padding as asc associate at
syn keyword sqlKeyword auto_increment avg batchsize begin between binary_double binary_float
syn keyword sqlKeyword binary_integer bit body break by byte call caller cascade case casespecific
syn keyword sqlKeyword cast character charset client close clustered cmp collect collection
syn keyword sqlKeyword column constant commit compress condition constraint continue
syn keyword sqlKeyword copy count big creation creator cs current schema cursor database data
syn keyword sqlKeyword datetime day days dec decimal declare default deferred defined definer
syn keyword sqlKeyword definition delimited delimiter desc diagnostics dir directory
syn keyword sqlKeyword distinct distribute do dynamic else elseif elsif enable end engine
syn keyword sqlKeyword escaped except exec execute exception exclusive exists exit fallback false
syn keyword sqlKeyword fetch fields file files for foreign format found from ftp full function
syn keyword sqlKeyword get global go group handler hash having hdfs host identity if ignore
syn keyword sqlKeyword immediate in include index initrans inner inout int int2 int4 int8
syn keyword sqlKeyword integer intersect interval into invoker is isopen items join keep key keys
syn keyword sqlKeyword language leave left like limit lines local location locator locators locks log
syn keyword sqlKeyword logged logging loop matched maxtrans merge message_text microsecond
syn keyword sqlKeyword microseconds multiset nchar new nvarchar no nocount nocompress nologging
syn keyword sqlKeyword none not notfound numeric number object off on only open or order out outer
syn keyword sqlKeyword outputformat over overwrite owner package partition partitioned pctfree pctused pls_integer
syn keyword sqlKeyword precision preserve primary print proc procedure qualify query_band quit
syn keyword sqlKeyword quoted_identifier raise real references regexp replace resignal restrict result
syn keyword sqlKeyword locator reduce return returns reverse right rlike role row rows rowtype
syn keyword sqlKeyword row_count rr rs pwd trim schema second seconds security segment sel
syn keyword sqlKeyword session sessions sets share signal simple_double simple_float simple_integer
syn keyword sqlKeyword smalldatetime sort sql sqlexception sqlinsert sqlstate sqlwarning stats
syn keyword sqlKeyword statistics step storage stored subdir substring sum sys_refcursor table
syn keyword sqlKeyword tablespace temporary terminated textimage_on then title to
syn keyword sqlKeyword top transaction true type union unique ur using value
syn keyword sqlKeyword values var varying volatile when where while with without work
syn keyword sqlKeyword abort xml yes activity_count cume_dist user dense_rank value lag
syn keyword sqlKeyword value lead string count loc rank row_number stdev
syn keyword sqlKeyword sysdate variance user admin option


syn keyword sqlOperator ! != $sum0 % & * + - / < <= <=> <> = == > >= ^ abs acos add_months aes_decrypt
syn keyword sqlOperator aes_encrypt array_contains ascii asin assert_true atan avg base64
syn keyword sqlOperator bin bloom_filter bround cardinality_violation cbrt ceil ceiling
syn keyword sqlOperator char_length character_length chr coalesce collect_list collect_set compute_stats
syn keyword sqlOperator concat concat_ws context_ngrams conv corr cos count covar_pop covar_samp crc32
syn keyword sqlOperator create_union cume_dist current_database current_date current_timestamp
syn keyword sqlOperator current_user date_add date_format date_sub datediff day dayofmonth dayofweek
syn keyword sqlOperator decode default.qtest_get_java_boolean degrees dense_rank div elt encode
syn keyword sqlOperator ewah_bitmap ewah_bitmap_and ewah_bitmap_empty ewah_bitmap_or exp explode
syn keyword sqlOperator extract_union factorial field find_in_set first_value floor floor_day floor_hour
syn keyword sqlOperator floor_minute floor_month floor_quarter floor_second floor_week floor_year
syn keyword sqlOperator format_number from_unixtime from_utc_timestamp get_json_object get_splits
syn keyword sqlOperator greatest grouping hash hex histogram_numeric hour if in in_bloom_filter in_file
syn keyword sqlOperator index initcap inline instr internal_interval isfalse isnotfalse isnotnull
syn keyword sqlOperator isnottrue isnull istrue java_method json_tuple lag last_day last_value lcase
syn keyword sqlOperator lead least length levenshtein likeall likeany ln locate log log10 log2
syn keyword sqlOperator logged_in_user lower lpad ltrim map map_keys map_values mask mask_first_n
syn keyword sqlOperator mask_hash mask_last_n mask_show_first_n mask_show_last_n matchpath max md5 min
syn keyword sqlOperator minute mod month months_between negative next_day ngrams noop
syn keyword sqlOperator noopstreaming noopwithmap noopwithmapstreaming ntile nullif nvl octet_length
syn keyword sqlOperator or parse_url parse_url_tuple percent_rank percentile percentile_approx pi pmod
syn keyword sqlOperator posexplode positive pow power printf quarter radians rand rank reflect reflect2
syn keyword sqlOperator regexp regexp_extract regexp_replace regr_avgx regr_avgy regr_count
syn keyword sqlOperator regr_intercept regr_r2 regr_slope regr_sxx regr_sxy regr_syy repeat replace
syn keyword sqlOperator replicate_rows reverse rlike round row_number rpad rtrim second sentences sha
syn keyword sqlOperator sha1 sha2 shiftleft shiftright shiftrightunsigned sign sin size sort_array
syn keyword sqlOperator sort_array_by soundex space split sq_count_check sqrt stack std stddev
syn keyword sqlOperator stddev_pop stddev_samp str_to_map substr substring substring_index sum
syn keyword sqlOperator tan to_date to_unix_timestamp to_utc_timestamp translate trim trunc ucase
syn keyword sqlOperator unbase64 unhex unix_timestamp upper uuid var_pop var_samp variance version
syn keyword sqlOperator weekofyear width_bucket windowingtablefunction xpath xpath_boolean
syn keyword sqlOperator xpath_double xpath_float xpath_int xpath_long xpath_number xpath_short
syn keyword sqlOperator xpath_string year

syn keyword sqlStatement alter analyze comment commit create
syn keyword sqlStatement delete drop explain grant insert lock
syn keyword sqlStatement rename revoke rollback select set
syn keyword sqlStatement truncate update describe load use


syn keyword sqlType tinyint smallint int bigint float double boolean string
syn keyword sqlType array map struct named_struct timestamp date varchar char

syn region sqlString start=+'+ skip=+\\'+ end=+'+ contains=Identifier
syn region sqlString start=+"+ skip=+\\"+ end=+"+ contains=Identifier

syn match sqlNumber +-\?\<[0-9]\++
syn match sqlNumber +-\?\<[0-9]\+\.[0-9]\++
syn keyword sqlNumber null

syn match Comment +--.*+
syn sync ccomment sqlComment

syn match hiveVar "hive\.[a-zA-Z.]\+"
syn match hiveVar "mapred\.[a-zA-Z.]\+"
syn match hiveVar "mapreduce\.[a-zA-Z.]\+"

hi link sqlKeyword  Identifier
hi link sqlNumber   Number
hi link sqlOperator Constant
hi link sqlStatement Statement
hi link sqlString   String
hi link sqlType     Type
hi link hiveVar     Special
