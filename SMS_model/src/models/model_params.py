from sklearn.pipeline import Pipeline
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import DecisionTreeClassifier

# from . import transforms as t
import transforms as t


rf = RandomForestClassifier(
    n_estimators=50,
    max_depth=10,
    random_state=42,
    class_weight='balanced'
)
# gs activity level = 90

rf_gs_params = {
    'activity_level__last_active': [15, 30, 60, 90],
    'clf__n_estimators': [10, 50],
    'clf__max_depth': [5, 10],
}

dt = DecisionTreeClassifier(
    max_depth=10,
    random_state=42,
    class_weight='balanced'
)

dt_gs_params = {
    'activity_level__last_active': [15, 30, 60, 90],
    'clf__max_depth': [5, 10, 20],
}


feature_pipeline = Pipeline([
    ('activity_level', t.ActivityLevel(last_active=90)),
    ('map_region', t.MapRegion()),
    ('get_dummies', t.GetDummies(
        dummy_cols=['post_type', 'source', 'cio_status',
                    'voter_registration_status', 'sms_status',
                    'region', 'activity_level'],
        cols_to_drop=['post_type', 'source', 'state', 'cio_status',
                      'voter_registration_status', 'sms_status',
                      'region', 'activity_level'],
    )),
    ('drop_cols', t.ColumnExcluder(cols=[
        'northstar_id', 'most_recent_signup', 'most_recent_post',
        'post_type_photo', 'post_type_share-social', 'post_type_text',
        'post_type_voter-reg',
        'source_niche', 'source_other', 'source_phoenix-oauth',
        # 'source_phoenix', 'source_phoenix-next',
        # 'source_sms',
        # 'sms_status_active', # 'sms_status_less', # 'sms_status_pending',
        'sms_status_unknown',
        'region_MIDDLE ATLANTIC', 'region_MIDWEST', 'region_NEW ENGLAND',
        'region_SOUTH', 'region_SOUTHWEST', 'region_WEST',
        # 'Animals', 'Bullying', 'Civic Action', 'Disasters', 'Discrimination',
        # 'Education', 'Environment', 'Homelessness', 'Mental Health',
        # 'Physical Health', 'Poverty', 'Relationships', 'Sex', 'Violence',
        # 'Week Of Action',
        'voter_registration_status_unregistered',
        # 'voter_registration_status_registered',
        # 'cio_status_customer_unsubscribed',
        'cio_status_customer_subscribed',
        'activity_level_no_action',
        'length_of_membership_days',
        # 'age',
    ])),
    ('clf', rf),
])


def __getattr__(self, name):
    return name
