select gu.user_id as 'northstar_id', ca.signup_id as 'signup_id', gc.campaign_run_id as 'campaign_run_id',
      ca.quantity as 'quantity', gc.id as 'contest_id', ca.status as 'reportback_status', ca.post_id as 'post_id'
      from gladiator.users gu
      left join gladiator.contest gc
        on gu.contest_id = gc.id
      left join quasar.campaign_activity ca
        on ca.northstar_id = gu.user_id and ca.campaign_id = gu.campaign_id and ca.campaign_run_id = gc.campaign_run_id
      group by ca.signup_id;;
    }