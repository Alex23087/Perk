from flask import Flask, request, jsonify
from flask_cors import CORS
import subprocess
import tempfile
import os
import json

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
    app.run(host="0.0.0.0", port=8080)
