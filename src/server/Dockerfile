FROM python:3.7

# set up apt-get repository
RUN sh -c 'echo > /etc/apt/sources.list.d/pgdg.list'
RUN sh -c 'echo "deb http://deb.debian.org/debian bullseye main contrib non-free\ndeb-src http://deb.debian.org/debian bullseye main contrib non-free\ndeb http://deb.debian.org/debian-security/ bullseye-security main contrib non-free\ndeb-src http://deb.debian.org/debian-security/ bullseye-security main contrib non-free\ndeb http://deb.debian.org/debian bullseye-updates main contrib non-free\ndeb-src http://deb.debian.org/debian bullseye-updates main contrib non-free\ndeb http://deb.debian.org/debian bullseye-backports main contrib non-free\ndeb-src http://deb.debian.org/debian bullseye-backports main contrib non-free" > /etc/apt/sources.list'


# install software
RUN apt-get update 
RUN apt-get install -y --allow-unauthenticated fish vim nano


COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

ENV STATIC_URL /static
ENV STATIC_PATH /gisdata

CMD [ "python3", "./manage.py" ]


