FROM alex23087/perkc:latest

RUN sudo apt-get update && sudo apt-get install -y python3 python3-pip

# Install Flask for the server
RUN sudo pip3 install flask flask-cors

# Copy server.py
COPY server.py /app/server.py
WORKDIR /app

EXPOSE 8080
CMD ["sudo", "python3", "server.py"]
