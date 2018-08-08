DROP MATERIALIZED VIEW IF EXISTS public.gtm_retention; 
CREATE MATERIALIZED VIEW public.gtm_retention AS
(SELECT
    a.northstar_id,
    a.signup_month,
    a.event_month,
    a.action_type,
    a.monthly_events,
    (DATE_PART('year', a.event_created_at::date) - DATE_PART('year', a.signup_created_at::date)) * 12 +
    (DATE_PART('month', a.event_created_at::date) - DATE_PART('month', a.signup_created_at::date)) as months_since_signup
  FROM
        (SELECT
          ca.northstar_id as northstar_id
        , date_trunc ('month', ca.signup_created_at) as signup_month
        , month_list.event_month as event_month
        , COALESCE(data.monthly_events, 0) as monthly_events
        , DATA.action_type AS action_type
        , ca.signup_created_at
        , DATA.timestamp AS event_created_at
        , row_number() over() AS key
        FROM
          public.campaign_activity ca
        LEFT JOIN
          (
            SELECT
              DISTINCT(date_trunc('month', member_event_log.timestamp)) as event_month
            FROM member_event_log
          ) as month_list
        ON month_list.event_month >= date_trunc ('month', ca.signup_created_at)
        LEFT JOIN
          (
            SELECT
                  mel.northstar_id
                , mel.timestamp as timestamp
                , mel.action_type AS action_type
                , date_trunc('month', mel.timestamp) as event_month
                , COUNT(distinct mel.event_id) AS monthly_events
            FROM member_event_log mel
            GROUP BY 1,2, 3
          ) as data
        ON data.event_month = month_list.event_month AND data.northstar_id = ca.northstar_id
        WHERE ca.campaign_run_id = '8022')
        a) ;;
CREATE INDEX ON public.gtm_retention (northstar_id);
GRANT SELECT ON public.gtm_retention TO looker;
GRANT SELECT ON public.gtm_retention TO dsanalyst;     