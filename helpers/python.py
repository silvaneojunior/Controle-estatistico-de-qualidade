import numpy as np

def get_max(x,axis=1,keepdims=False):
  assert axis>0, ValueError('Axis should be a positive intenger.')
  return np.max(x,axis=int(axis-1),keepdims=keepdims)

def get_min(x,axis=1,keepdims=False):
  assert axis>0, ValueError('Axis should be a positive intenger.')
  return np.min(x,axis=int(axis-1),keepdims=keepdims)

def get_mean(x,axis=1,keepdims=False):
  assert axis>0, ValueError('Axis should be a positive intenger.')
  return np.mean(x,axis=int(axis-1),keepdims=keepdims)

def get_std(x,axis=1,keepdims=False):
  assert axis>0, ValueError('Axis should be a positive intenger.')
  return np.std(x,axis=int(axis-1),keepdims=keepdims,ddof=1)

def get_sum(x,axis=1,keepdims=False):
  assert axis>0, ValueError('Axis should be a positive intenger.')
  return np.sum(x,axis=int(axis-1),keepdims=keepdims)
