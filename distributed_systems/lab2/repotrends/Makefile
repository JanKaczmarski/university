VENV    := .venv
PYTHON  := $(VENV)/bin/python
PYTEST  := $(VENV)/bin/pytest
UVICORN := $(VENV)/bin/uvicorn
PIP     := $(VENV)/bin/pip

.PHONY: install run


$(VENV):
	python3 -m venv $(VENV)

install: $(VENV)
	$(PIP) install -r requirements.txt -i https://pypi.org/simple

run:
	$(UVICORN) app:app --host 0.0.0.0 --port 8000 --reload
