from flask import Flask, request, jsonify
from flask_cors import CORS
import subprocess
import tempfile
import os
import json
import ssl

app = Flask(__name__)
CORS(app)

@app.route("/compile", methods=["POST"])
def compile_perk():
    code = request.json.get("code")
    if not code:
        return jsonify({"error": "No code provided"}), 400

    with tempfile.TemporaryDirectory() as tmpdir:
        perk_file = os.path.join(tmpdir, "main.perk")
        c_file = os.path.join(tmpdir, "main.c")
        binary_file = os.path.join(tmpdir, "main.out")

        with open(perk_file, "w") as f:
            f.write(code)

        try:
            # Try to compile Perk code with timeout
            subprocess.run(["perkc", perk_file, "-o", c_file], 
                          check=True, timeout=5)

            # Compile the generated C code with timeout
            subprocess.run(["gcc", c_file, "-o", binary_file], 
                          check=True, timeout=5)

            # Run the binary and capture output with timeout
            result = subprocess.run([binary_file], 
                                   capture_output=True, text=True, timeout=5)
            return jsonify({"output": result.stdout})
        
        except subprocess.TimeoutExpired:
            return jsonify({"error": "Compilation or execution timed out (5 seconds limit)"}), 504
        
        except subprocess.CalledProcessError as e:
            # If compilation fails, run --check to get error message
            try:
                check_proc = subprocess.run(
                    ["perkc", "--check", perk_file],
                    capture_output=True, text=True, timeout=5
                )
                if check_proc.stdout:
                    try:
                        error_json = json.loads(check_proc.stdout)
                        if "file" in error_json:
                            del error_json["file"]
                        return jsonify(error_json), 400
                    except json.JSONDecodeError:
                        return jsonify({"error": check_proc.stdout}), 400
                else:
                    return jsonify({"error": check_proc.stderr or f"Process failed with exit code {e.returncode}"}), 500
            except subprocess.TimeoutExpired:
                return jsonify({"error": "Error checking timed out"}), 504
            except Exception as check_error:
                return jsonify({"error": f"Failed to get error details: {str(check_error)}"}), 500

@app.route("/pulse", methods=["GET"])
def pulse():
    return jsonify({"status": "ok"})

if __name__ == "__main__":
    # Get certificate paths from environment variables
    cert_file = os.environ.get('SSL_CERT_PATH', '/certs/server.crt')
    key_file = os.environ.get('SSL_KEY_PATH', '/certs/server.key')
    port = int(os.environ.get('PORT', '8443'))
    
    # Create SSL context
    ssl_context = None
    
    if os.path.exists(cert_file) and os.path.exists(key_file):
        try:
            context = ssl.SSLContext(ssl.PROTOCOL_TLSv1_2)
            context.load_cert_chain(cert_file, key_file)
            ssl_context = context
            print(f"Using SSL certificates: {cert_file}, {key_file}")
        except Exception as e:
            print(f"Failed to load certificates {cert_file}, {key_file}: {e}")
    
    if ssl_context:
        print(f"Starting HTTPS server on port {port}")
        app.run(host="0.0.0.0", port=port, ssl_context=ssl_context)
    else:
        fallback_port = int(os.environ.get('HTTP_PORT', '8080'))
        print(f"No SSL certificates found, starting HTTP server on port {fallback_port}")
        app.run(host="0.0.0.0", port=fallback_port)
