from enum import Enum

from fastapi import FastAPI

app = FastAPI()


# sample requests and queries
@app.get("/")
async def root():
    return {"message": "Hello World"}


# sample path paramters => entries in URL
@app.get("/hello/{name}")
async def say_hello(name: str):
    return {"message": f"Hello {name}"}


# Path parameters predefined values
# https://fastapi.tiangolo.com/tutorial/path-params/
class ModelName(str, Enum):
    alexnet = "alexnet"
    resnet = "resnet"
    lenet = "lenet"


@app.get("/v1/models/{model_name}")
async def get_model(model_name: ModelName):
    if model_name is ModelName.alexnet:
        return {"model_name": model_name, "message": "Deep Learning FTW!"}
    if model_name.value == "lenet":
        return {"model_name": model_name, "message": "LeCNN all the images"}
    return {"model_name": model_name, "message": "Have some residuals"}


# query parametres are added as elements to the url e.g. items?skip=10&limit=3
# https://fastapi.tiangolo.com/tutorial/query-params/
fake_items_db = [{"item_name": "Foo"}, {"item_name": "Bar"}, {"item_name": "Baz"}]


@app.get("/v2/items")
async def read_item(skip: int = 0, limit: int = 10):
    return fake_items_db[skip : skip + limit]


# Optional parameters added to query, one of the element in Union
from typing import Optional, Union

# In this case, there are 3 query parameters:
# needy, a required str.
# skip, an int with a default value of 0.
# limit, an optional int.


@app.get("/v3/items/{item_id}")
async def read_user_item(item_id: str, needy: str, skip: int = 0, limit: Union[int, None] = None):
    item = {"item_id": item_id, "needy": needy, "skip": skip, "limit": limit}
    return item


# if you want to send it as a request body you have to define the class inheritet from pydantic base model
# Request Body
# https://fastapi.tiangolo.com/tutorial/body/
from pydantic import BaseModel, Field, validator


class Item(BaseModel):
    name: str
    description: Union[str, None] = None
    price: float
    tax: Union[float, None] = None


# create model
@app.post("/v4/items/")
async def create_item(item: Item):
    return item


# using model


@app.post("/v5/items/")
async def create_item(item: Item):
    item_dict = item.dict()
    if item.tax:
        price_with_tax = item.price + item.tax
        item_dict.update({"price_with_tax": price_with_tax})
    return item_dict


# all together


@app.put("/v6/items/{item_id}")
async def create_item(item_id: int, item: Item, q: Union[str, None] = None):
    result = {"item_id": item_id, **item.dict()}
    if q:
        result.update({"q": q})
    return result


# If the parameter is also declared in the path, it will be used as a path parameter.
# If the parameter is of a singular type (like int, float, str, bool, etc) it will be interpreted as a query parameter.
# If the parameter is declared to be of the type of a Pydantic model, it will be interpreted as a request body.

# additional status code:
# https://fastapi.tiangolo.com/advanced/additional-status-codes/

from fastapi import Body, FastAPI, HTTPException, status
from fastapi.responses import JSONResponse

items = {"foo": {"name": "Fighters", "size": 6}, "bar": {"name": "Tenders", "size": 3}}


@app.put("/v7/items/{item_id}")
async def upsert_item(
    item_id: str,
    name: Union[str, None] = Body(default=None),
    size: Union[int, None] = Body(default=None),
):
    if item_id in items:
        item = items[item_id]
        item["name"] = name
        item["size"] = size
        return item
    else:
        item = {"name": name, "size": size}
        items[item_id] = item
        return JSONResponse(status_code=status.HTTP_201_CREATED, content=item)


@app.delete("/v8/items/delete")
async def delete_and_error(error: int):
    return_content = ""
    if error >= 400 and error < 500:
        return JSONResponse(status_code=status.HTTP_404_NOT_FOUND, content=return_content)
    elif error >= 500 and error < 600:
        return JSONResponse(status_code=status.HTTP_503_SERVICE_UNAVAILABLE, content=return_content)
    else:
        return JSONResponse(status_code=status.HTTP_501_NOT_IMPLEMENTED, content=return_content)


# ------ SOLUTION -------

# key-value store
# TODO: Change to sqlite for persistent storage
polls = {}


class Poll(BaseModel):
    name: str = Field(..., min_length=1, max_length=100, description="Unique poll name")
    upvotes: int = Field(default=0, ge=0, description="Number of upvotes")
    downvotes: int = Field(default=0, ge=0, description="Number of downvotes")
    description: Optional[str] = Field(default=None, max_length=500, description="Optional poll description")


@app.post("/v1/poll", response_model=Poll, status_code=status.HTTP_201_CREATED)
def create_poll(poll: Poll):
    if poll.name in polls:
        if poll != polls[poll.name]:
            raise HTTPException(
                status_code=status.HTTP_409_CONFLICT,
                detail="poll already exists and has a different config",
            )
        return polls[poll.name]  # idempotency

    polls[poll.name] = poll
    return poll


@app.get("/v1/polls", response_model=list[Poll])
def get_all_polls():
    return list(polls.values())


@app.get("/v1/poll/{poll_name}", response_model=Poll)
def get_poll(poll_name: str):
    if poll_name not in polls:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="poll doesn't exist")

    return polls[poll_name]


@app.put("/v1/poll/{poll_name}", response_model=Poll)
def update_poll(poll_name: str, new_pol: Poll):
    if poll_name != new_pol.name:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="provided poll name doesn't match body Poll",
        )

    if poll_name not in polls:  # upsert
        polls[poll_name] = new_pol
        return JSONResponse(status_code=status.HTTP_201_CREATED, content=new_pol.model_dump())
    else:
        # allow only the modification of description - not votes
        old_upvotes, old_downvotes = polls[poll_name].upvotes, polls[poll_name].downvotes

        # don't allow the modification of vote count
        new_pol.upvotes = old_upvotes
        new_pol.downvotes = old_downvotes

        polls[poll_name] = new_pol
        return new_pol


@app.delete("/v1/poll/{poll_name}", status_code=status.HTTP_204_NO_CONTENT)
def delete_poll(poll_name: str):
    if poll_name not in polls:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="poll doesn't exist")

    del polls[poll_name]


@app.put("/v1/poll/{poll_name}/upvote", response_model=Poll)
def poll_upvote(poll_name: str):
    if poll_name not in polls:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="poll doesn't exist")

    poll = polls[poll_name]
    poll.upvotes += 1
    return poll


@app.put("/v1/poll/{poll_name}/downvote", response_model=Poll)
def poll_downvote(poll_name: str):
    if poll_name not in polls:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="poll doesn't exist")

    poll = polls[poll_name]
    poll.downvotes += 1
    return poll
