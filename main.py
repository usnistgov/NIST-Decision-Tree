from fastapi import FastAPI
from pydantic import BaseModel
from typing import Literal
import subprocess
import json
import uuid

class NDT_data(BaseModel):
    laboratory: list[str] = ['a','b','c']
    measured_values: list[float] = [1,2,3]
    std_unc: list[float] = [1,1,1]
    dof: list[int] | None = [100,100,100]
    exclude: list[bool] | None = [False,False,False]
    procedure: str | None = 'Recommended'
    num_bootstrap: int | None = 1000
    seed: int | None = 123
    n_iter: int | None = 50000
    burn_in: int | None = 25000

class TestData(BaseModel):
    measured_values: list[float] = [1,2,3]
    std_unc: list[float] = [1,1,1]
    dof: list[int] | None = [100,100,100]
    exclude: list[bool] | None = [False,False,False]
    test: str = Literal['homogeneity','normality','symmetry']

app = FastAPI()

@app.get('/health-check/')
async def root():
    return {"message": "Server is up and running."}

@app.post('/run-ndt/')
async def run_ndt(req: NDT_data):
    return req

@app.post('/run-test/')
async def run_test(req: TestData):

    # uid = str(uuid.uuid4())
    input_path = f"tmp_input.json"
    output_path = f"tmp_output.json"

    with open(input_path, 'w') as f:
        json.dump(req.model_dump(), f)

    result = subprocess.run(
        ["C:/Users/dtn1/AppData/Local/Programs/R/R-4.3.1/bin/Rscript.exe", "fastapi_files/run_test.R", 
         input_path, output_path],
         capture_output=True,
         text=True,
         check=True
    )

    with open(output_path, 'r') as f:
        response = json.load(f)

    return response
