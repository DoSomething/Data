select SUM(temp.quantity)
from
	(select s.id, s.quantity
	from signups s
	left join posts p 
		on p.signup_id = s.id
	where s.campaign_run_id = 7872
	and s.quantity IS NOT NULL 
	and p.status = 'accepted'
	group by s.id
	) temp;
 