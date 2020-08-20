; Read name out loud

FUNCTION hash_or_dict_to_struct,input_hash_or_dict
  struct = {}
  foreach value,input_hash_or_dict,key DO struct = create_struct(struct,key,value)
  return,struct
END

