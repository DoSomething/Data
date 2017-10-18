import os
from customerio import CustomerIO

site_id = "c61296777a7153952acf"
api_key = os.environ.get("CUSTOMER_IO_TOKEN")

cio = CustomerIO(site_id, api_key)

cio.identify('59e61fefa0bfad5a66294b9a')