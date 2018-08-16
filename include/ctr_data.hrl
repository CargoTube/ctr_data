


-record(ctr_realm, {name = undefined,
                    authmethods = [],
                    authmapping = []
                   }).

-record(ctr_subscription, {
          id = undefined,
          realm = undefined,
          uri = undefined,
          match = exact,
          created = undefined,
          subscribers = []
         }).

-record(ctr_publication, {
          id = undefined,
          pub_sess_id = undefined,
          options = undefined,
          details = #{},
          subs = [],
          realm = undefined,
          topic = undefined,
          ts = undefined,
          arguments = undefined,
          argumentskw = undefined
         }).

-record(ctr_registration, {
          id = undefined,
          realm = undefined,
          procedure = undefined,
          match = exact,
          invoke = unknown,
          created = undefined,
          callee_sess_ids = []
         }).

-record(ctrd_invocation, {
          id = undefined,
          caller_sess_id = undefined,
          caller_req_id = undefined,
          reg_id = undefined,
          ts = undefined,
          procedure = <<>>,
          options = #{},
          arguments = undefined,
          argumentskw = undefined,
          callees = [],
          results = [],
          realm = undefined
         }).
