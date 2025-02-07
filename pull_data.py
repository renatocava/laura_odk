from pyodk.client import Client

with Client(config_path=".pyodk_config.toml") as client:
    # print(client.projects.list())
    # print(client.forms.list())
    print(client.submissions.get_table(form_id="Laura2-piloto-encuesta-preregistro-v1")["value"])