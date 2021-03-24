Select distinct campaign_node_id, 
campaign_node_id_title, 
campaign_node_id_title as 'campaign',
campaign_action_type as 'action type', 
campaign_cause_type as 'cause type',
campaign_cta as 'call to action', 
DATE_FORMAT(campaign_run_start_date, '%m/%d/%Y') as 'startdate', 
DATE_FORMAT (campaign_run_end_date, '%m/%d/%Y') as 'enddate'
from quasar.campaign_info
