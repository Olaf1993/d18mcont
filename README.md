# d18mcont




build:

sudo docker build -t d18mcont .


run

sudo docker run --rm -p 3838:3838 -v /home/mint/Dokumente/d18m_cont/app/:/srv/shiny-server/ -v /home/mint/Schreibtisch/logs/:/var/log/shiny-server/ -v /home/mint/Dokumente/d18m_cont/db/:/db/ d18mcont


chmod 777 /home/mint/Dokumente/d18m_cont/db/


