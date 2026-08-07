FROM grafana/grafana:10.4.0
ENV GF_PATHS_PROVISIONING=/etc/grafana/provisioning
ENV GF_SECURITY_ALLOW_EMBEDDING=true
USER root
COPY grafana/provisioning /etc/grafana/provisioning
COPY grafana/import-dashboards.sh /usr/local/bin/import-dashboards.sh
RUN chmod +x /usr/local/bin/import-dashboards.sh
USER grafana
EXPOSE 3000
