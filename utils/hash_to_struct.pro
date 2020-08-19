; RTFC

FUNCTION hash_to_struct,input_hash
  struct = {}
  foreach value,input_hash,key DO struct = create_struct(struct,key,value)
  return,struct
END
