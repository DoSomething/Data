SELECT
    step1.signup_created_at AS step1, -- intitial co-reg sms
    step2.submission_created_at AS step2, -- rb on co-reg
    step3.signup_created_at AS step3, -- signs up for another campaign
    step1.northstar_id
  


        
        SELECT
          step1.signup_created_at AS step1, -- intitial co-reg sms
          step2.submission_created_at AS step2, -- rb on co-reg
          sms.signup_created_at as step3
        FROM 
            (SELECT ca.northstar_id, ca.signup_id, ca.signup_created_at
            FROM quasar.campaign_activity ca
            WHERE ca.signup_source = 'niche'
            GROUP BY ca.northstar_id) AS step1
            LEFT JOIN
          -- Step 2 query
            (SELECT ca.northstar_id, ca.signup_id, ca.submission_created_at
            FROM quasar.campaign_activity ca
            WHERE ca.signup_source = 'niche'
            AND ca.post_id > 0)  AS step2
            ON step2.northstar_id = step1.northstar_id AND step1.signup_id = step2.signup_id
        LEFT JOIN
            (SELECT 
              step3.northstar_id, step3.signup_id, step3.signup_created_at
            FROM
                -- Step 1 query
                (SELECT ca.northstar_id, ca.signup_id, ca.signup_created_at
                 FROM quasar.campaign_activity ca
                 WHERE ca.signup_source = 'niche'
                 GROUP BY ca.northstar_id) AS step1
               LEFT JOIN
                -- Step 3 query
                (SELECT ca.northstar_id, ca.signup_id, ca.signup_created_at
                 FROM quasar.campaign_activity ca
                 WHERE ca.signup_source = 'sms') AS step3
               ON step3.northstar_id = step1.northstar_id
            WHERE year(step1.signup_created_at) >= '2016'
            AND step1.signup_created_at < step3.signup_created_at) 
            sms -- this is to exclude people who signed up on sms before they came in from sms. I know, crazy!
        ON step1.northstar_id = sms.northstar_id
          
